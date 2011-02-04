
(cl:defpackage #:retrospectiff2
  (:use #:cl
        #:com.gigamonkeys.binary-data
        #:com.gigamonkeys.binary-data.common-datatypes))

(in-package :retrospectiff2)

(defparameter *byte-order* nil)

;;; Perhaps the next few types should be moved to a
;;; binary-data-extensions file or some such?
(define-binary-type array (type size)
  (:reader (in)
           (let ((arr (make-array size :element-type type)))
             (dotimes (i size)
               (setf (elt arr i)
                     (read-value type in)))
             arr))
  (:writer (out value)
           (dotimes (i (length value))
             (write-value type out (elt value i)))))

(define-binary-type unsigned-integer* (bytes bits-per-byte)
  (:reader (in)
           (case *byte-order*
             (:big-endian
              (loop with value = 0
                 for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return value)))
             (:little-endian
              (loop with value = 0
                 for low-bit to (* bits-per-byte (1- bytes)) by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return value)))))
  (:writer (out value)
           (case *byte-order*
             (:big-endian
              (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))
             (:little-endian
              (loop for low-bit to  (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))))

(define-binary-type u2* () (unsigned-integer* :bytes 2 :bits-per-byte 8))
(define-binary-type u4* () (unsigned-integer* :bytes 4 :bits-per-byte 8))

;;; end binary-data-extensions section
;;;

(define-binary-type tiff-byte-order ()
  (:reader (in)
           (let ((val (read-value 'u2 in)))
             (case val
               (#x4949 (setf *byte-order* :little-endian))
               (#x4D4D (setf *byte-order* :big-endian))
               (t (error "unknown byte order")))))
  (:writer (out value)
           (case (or *byte-order* value)
             (:little-endian
              (write-value 'u2 out #x4949))
             (:big-endian
              (write-value 'u2 out #x4d4d)))))

(define-tagged-binary-class ifd-entry ()
  ((tag u2*)
   (field-type u2*)
   (value-count u4*))
  (:dispatch (case field-type
               (1 (if (<= value-count 4)
                      'inline-byte-ifd-entry
                      'non-inline-byte-ifd-entry))
               (2 (if (<= value-count 4)
                      'inline-ascii-ifd-entry
                      'non-inline-ascii-ifd-entry))
               (3 (if (<= value-count 2)
                      'inline-short-ifd-entry
                      'non-inline-short-ifd-entry))
               #+nil
               (progn (2 'ascii-ifd-entry)
                      (3 'short-ifd-entry)
                      (4 'long-ifd-entry)
                      (5 'rational-ifd-entry)
                      (6 'sbyte-ifd-entry)
                      (7 'undefined-ifd-entry)
                      (8 'sshort-ifd-entry)
                      (9 'slong-ifd-entry)
                      (10 'srational-ifd-entry)
                      (11 'float-ifd-entry)
                      (12 'double-ifd-entry))
               (t 'unknown-ifd-entry))))

(define-binary-class unknown-ifd-entry (ifd-entry)
  ((value-offset u4*)))

(define-binary-class inline-ifd-entry (ifd-entry) ())

(define-binary-class non-inline-ifd-entry (ifd-entry)
  ((offset u4*)))

(defparameter *binary-type-sizes*
  `((iso-8859-1-char . 1)
    (u1 . 1)
    (u2 . 2)
    (u4 . 4)))

(define-binary-type inline-array (type size position element-type)
  (:reader (in)
           (let* ((bytes-per-element (cdr (assoc type *binary-type-sizes*))))
             (let ((pad (- (/ 4 bytes-per-element) size)))
               (when (minusp pad)
                 (error "tried to read more than 4 bytes inline!"))
               (prog1
                   (let ((v (apply #'make-array size
                                   (when element-type `(:element-type ,element-type)))))
                     (loop for i below size
                        do (setf (elt v i) (read-value type in)))
                     v)
                 (loop for i below pad do (read-value type in))))))
  (:writer (out value)
           (let* ((bytes-per-element (cdr (assoc type *binary-type-sizes*))))
             (let ((pad (- (/ 4 bytes-per-element) size)))
               (when (minusp pad)
                 (error "tried to write more than 4 bytes inline!"))
               (loop for x across value
                  do (write-value type x out))
               (loop for i below pad
                  do (write-value type 0 out))))))

(define-binary-type non-inline-array (type size position element-type)
  (:reader (in)
           (let ((cur (file-position in)))
             (file-position in position)
             (prog1 
                 (let ((v (apply #'make-array size
                                 (when element-type `(:element-type ,element-type)))))
                   (loop for i below size
                      do (setf (elt v i) (read-value type in)))
                   v)
               (file-position in cur))))
  (:writer (out value)
           (let ((cur (file-position out)))
             (file-position out position)
             (loop for x across value
                do (write-value type x out))
             (file-position out cur))))
;;
;; 1 - BYTE
(define-binary-class inline-byte-ifd-entry (ifd-entry)
  ((data (inline-array :type 'u1 :size value-count))))

(define-binary-class non-inline-byte-ifd-entry (non-inline-ifd-entry)
  ((data (non-inline-array :type 'u1 :size value-count :position offset))))

;; 2 - ASCII
(define-binary-class inline-ascii-ifd-entry (ifd-entry)
  ((data (inline-array :type 'iso-8859-1-char :size value-count
                       :element-type 'character))))

(define-binary-class non-inline-ascii-ifd-entry (non-inline-ifd-entry)
  ((data (non-inline-array :type 'iso-8859-1-char :size value-count
                           :position offset :element-type 'character))))

;;
;; 3
(define-binary-class inline-short-ifd-entry (ifd-entry)
  ((data (inline-array :type 'u2 :size value-count))))

(define-binary-class non-inline-short-ifd-entry (non-inline-ifd-entry)
  ((data (non-inline-array :type 'u2 :size value-count :position offset))))


;; 4
(define-binary-class inline-long-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 5
(define-binary-class inline-rational-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 6
(define-binary-class inline-sbyte-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 7
(define-binary-class inline-undefined-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 8
(define-binary-class inline-sshort-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 9
(define-binary-class inline-slong-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 10
(define-binary-class inline-srational-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 11
(define-binary-class inline-float-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 12
(define-binary-class inline-double-ifd-entry (ifd-entry)
  ((value-offset u4*)))


(define-binary-type tiff-ifd-offset ()
  (:reader (in)
           (let ((val (read-value 'u4* in)))
             (prog1 val
               (if (plusp val)
                   (file-position in val)))))
  (:writer (out value)
           (write-value 'u4* out value)
           (if (plusp value)
               (file-position out value))))

(define-binary-class ifd ()
  ((entry-count u2*)
   (entries (array :type 'ifd-entry :size entry-count))
   (next-ifd-offset tiff-ifd-offset)))

(define-binary-type ifd-list ()
  (:reader (in)
           (loop for ifd = (read-value 'ifd in)
              collect ifd
              while (plusp (next-ifd-offset ifd))))
  (:writer (out value)
           (loop for ifd in value
              do (write-value 'ifd out ifd))))

(define-binary-class tiff ()
  ((byte-order tiff-byte-order)
   (magic u2*)
   (ifd-offset tiff-ifd-offset)
   (ifd-list ifd-list)))

(defmethod read-value :around ((type (eql 'tiff)) stream &key)
  (let (*byte-order*)
    (call-next-method)))

(defun read-tiff-stream (stream)
  (read-value 'tiff stream))

(defun write-tiff-stream (stream obj &key (byte-order :big-endian))
  (let ((*byte-order* byte-order))
    (write-value 'tiff stream obj)))

