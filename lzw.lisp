
(in-package :retrospectiff)

(defconstant +clear-code+ 256)
(defconstant +end-of-information-code+ 257)
(defconstant +first-entry+ 258)
(defconstant +last-code+ 4093)
(defconstant +max-code+ 4094)

(defun ensure-array-size-and-set-fill-pointer (array fill-pointer)
  (let ((length (array-dimension array 0)))
    (when (>= fill-pointer length)
      (adjust-array array (max 256
                               (+ length (ash length -1))
                               fill-pointer)))
    (setf (fill-pointer array) fill-pointer)))

(defun lzw-encode (raw-vector)
  (declare (optimize (debug 2)))
  (let ((vector-hash)
        (next-entry)
        (bit-count 0)
        (output (make-array 256
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0
                            :adjustable t)))
    (labels ((initialize-vector-table ()
               (setf vector-hash (make-hash-table :test 'equalp)
                     next-entry +first-entry+))
             (lookup-vector (vector)
               (cond ((= (length vector) 1)
                      (char-code (elt vector 0)))
                     ((> (length vector) 1)
                      (gethash vector vector-hash))
                     (t (error "huh?"))))
             (add-vector (vector)
               (setf (gethash vector vector-hash) next-entry)
               (incf next-entry))
             (code-size ()
               (integer-length next-entry))
             (write-code (int)
               (let ((new-fill-pointer
                      (1+ (ash (+ bit-count (code-size)) -3))))
                 (ensure-array-size-and-set-fill-pointer
                  output new-fill-pointer)
                 (set-bits output bit-count (+ bit-count (code-size))
                           int))
               (incf bit-count (code-size))
               (when (= int +clear-code+)
                 (progn
                   ;; FIXME
                   ;; do something special here
                   ))))
      (initialize-vector-table)
      (write-code +clear-code+)
      (let ((omega))
        (loop for k across raw-vector
           do
             (let ((cat (concatenate 'vector omega (vector k))))
               (if (lookup-vector cat)
                   (setf omega cat)
                   (progn
                     (write-code (lookup-vector omega))
                     (add-vector cat)
                     (setf omega (vector k))
                     (when (= next-entry 4093)
                       (write-code +clear-code+))))))
        (write-code (lookup-vector omega)))
      (write-code +end-of-information-code+)
      output)))

(defconstant +initial-code-size+ 9)

;; while ((Code = GetNextCode()) != EoiCode) {
;;     if (Code == ClearCode) { 
;;         InitializeTable(); 
;;         Code = GetNextCode(); 
;;         if (Code == EoiCode) 
;;             break; 
;;         WriteString(StringFromCode(Code)); 
;;         OldCode = Code; 
;;     }  /* end of ClearCode case */ 
;;     else { 
;;         if (IsInTable(Code)) { 
;;             WriteString(StringFromCode(Code)); 
;;             AddStringToTable(StringFromCode(OldCode) +
;;                              FirstChar(StringFromCode(Code))); 
;;             OldCode = Code; 
;;         } else { 
;;             OutString = StringFromCode(OldCode) + 
;;                 FirstChar(StringFromCode(OldCode)); 
;;             WriteString(OutString); 
;;             AddStringToTable(OutString); 
;;             OldCode = Code; 
;;         } 
;;     } /* end of not-ClearCode case */ 
;;  } /* end of while loop */ 

#+:lzw-debugging
(defun lzw-decode-codes (compressed-vector)
  (declare (optimize (debug 2)))
  (let ((bit-offset 0)
        (code-size +initial-code-size+))
    (labels ((get-next-code ()
               (get-bits compressed-vector
                         bit-offset
                         (incf bit-offset code-size))))
      (loop for code = (get-next-code)
         while (not (= code +end-of-information-code+))
         collect code))))

(defun lzw-decode (compressed-vector &key stream)
  (declare (optimize (debug 2)))
  (let ((input-bit-offset 0)
        (output-byte-offset 0)
        (code-size +initial-code-size+)
        (next-entry +first-entry+)
        vector-hash
        (output (make-array 256
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0
                            :adjustable t)))
    (flet ((initialize-vector-table ()
             (setf vector-hash (make-hash-table :test 'equal)
                   next-entry +first-entry+))
           (get-next-code ()
             (get-bits compressed-vector
                       input-bit-offset
                       (incf input-bit-offset code-size)))
           (vector-from-code (code)
             (if (< code +clear-code+)
                 (vector code)
                 (gethash code vector-hash)))
           (add-vector-to-table (vector)
             (setf (gethash next-entry vector-hash) vector)
             (incf next-entry))
           (write-code (int)
             (ensure-array-size-and-set-fill-pointer
              output output-byte-offset)
             (setf (aref output output-byte-offset) int)
             (setf (fill-pointer output)
                   (incf output-byte-offset))))
      (flet ((write-decoded-vector (vector)
               (loop for char across vector
                  do 
                    (if stream
                        (write-byte char stream)
                        (write-code char)))))
        (loop for code = (get-next-code)
           with old-code
           while (not (= code +end-of-information-code+))
           do 
           (cond ((= code +clear-code+)
                  (initialize-vector-table))
                 (t
                  (let ((string (vector-from-code code)))
                    (write-decoded-vector string)
                    (when old-code
                      (add-vector-to-table
                       (concatenate 'vector
                                    (vector-from-code old-code)
                                    (vector (elt string 0))))))
                  (setf old-code code))))))
    (unless stream output)))

