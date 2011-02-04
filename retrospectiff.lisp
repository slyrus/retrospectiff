;;; file: retrospectiff.lisp
;;; author: cyrus harmon
;;;
;;; Copyright (c) 2008-2011 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :retrospectiff)

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
(define-binary-type u8* () (unsigned-integer* :bytes 8 :bits-per-byte 8))

(defun convert-to-signed-integer (num bits)
  (let ((max (1- (ash 1 (1- bits)))))
    (if (> num max)
        (lognot (- (1- (ash 1 bits)) num))
        num)))

(defun convert-to-unsigned-integer (num bits)
  (if (minusp num)
      (+ (ash 1 bits) num)
      num))

(define-binary-type signed-integer* (bytes bits-per-byte)
  (:reader (in)
           (convert-to-signed-integer
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
                  finally (return value))))
            (* bytes bits-per-byte)))
  (:writer (out value)
           (let ((value (convert-to-unsigned-integer value (* bytes bits-per-byte))))
             (case *byte-order*
               (:big-endian
                (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                   do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))
               (:little-endian
                (loop for low-bit to  (* bits-per-byte (1- bytes)) by bits-per-byte
                   do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))))))

(define-binary-type s1* () (signed-integer* :bytes 1 :bits-per-byte 8))
(define-binary-type s2* () (signed-integer* :bytes 2 :bits-per-byte 8))
(define-binary-type s4* () (signed-integer* :bytes 4 :bits-per-byte 8))

(define-binary-type f4* ()
  (:reader (in)
           (ieee-floats:decode-float32 (read-value 'u4* in)))
  (:writer (out value)
           (write-value 'u4* out (ieee-floats:encode-float32 value))))

(define-binary-type f8* ()
  (:reader (in)
           (ieee-floats:decode-float64 (read-value 'u8* in)))
  (:writer (out value)
           (write-value'u8* out (ieee-floats:encode-float64 value))))

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
               (1 'byte-ifd-entry)
               (2 'ascii-ifd-entry)
               (3 'short-ifd-entry)
               (4 'long-ifd-entry)
               (5 'rational-ifd-entry)
               (6 'sbyte-ifd-entry)
               (8 'sshort-ifd-entry)
               (9 'slong-ifd-entry)
               (10 'srational-ifd-entry)
               (11 'float-ifd-entry)
               (12 'double-ifd-entry)
               (t 'unknown-ifd-entry))))

(define-binary-class unknown-ifd-entry (ifd-entry)
  ((value-offset u4*)))

(defparameter *binary-type-sizes*
  `((iso-8859-1-char . 1)
    (u1 . 1)
    (u2 . 2)
    (u4 . 4)
    (u2* . 2)
    (u4* . 4)
    (s1* . 1)
    (s2* . 2)
    (s4* . 4)
    (rational . 8)
    (srational . 8)
    (f4* . 4)
    (f8* . 8)))

(define-binary-type ifd-array (type size element-type)
  (:reader (in)
           (let* ((bytes-per-element (cdr (assoc type *binary-type-sizes*))))
             (let ((pad (- (/ 4 bytes-per-element) size))
                   (v (apply #'make-array size
                             (when element-type `(:element-type ,element-type)))))
               (if (minusp pad)
                   (let ((position (read-value 'u4* in))
                         (cur (file-position in)))
                     (file-position in position)
                     (loop for i below size
                        do (setf (elt v i) (read-value type in)))
                     (file-position in cur))
                   (progn
                     (loop for i below size
                        do (setf (elt v i) (read-value type in)))
                     (loop for i below pad do (read-value type in))))
               v)))
  (:writer (out value)
           (let* ((bytes-per-element (cdr (assoc type *binary-type-sizes*))))
             (let ((pad (- (/ 4 bytes-per-element) size)))
               (if (minusp pad)
                   (let ((position 0)
                         (cur (file-position out)))
                     (declare (ignore position cur))
                     (error "this is broken until we figure out how to compute position!")
                     #+nil
                     (progn (file-position out position)
                            (loop for x across value
                               do (write-value type x out))
                            (file-position out cur)))
                   (progn (loop for x across value
                             do (write-value type x out))
                          (loop for i below pad
                             do (write-value type 0 out))))))))

;; 1 - byte
(define-binary-class byte-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u1 :size value-count))))

;; 2 - ascii
(define-binary-class ascii-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'iso-8859-1-char :size value-count :element-type 'character))))

;; 3 -- short
(define-binary-class short-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u2* :size value-count))))

;; 4 -- long
(define-binary-class long-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u4* :size value-count))))

;; 5 -- rational
(define-binary-class rational ()
  ((numerator u4*)
   (denominator u4*)))

(define-binary-class rational-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'rational :size value-count))))

;; 6 -- signed byte
(define-binary-class sbyte-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's1* :size value-count))))

;; 7 -- undefined (and unused, at least for now)
#+nil
(define-binary-class undefined-ifd-entry (ifd-entry)
  ((value-offset u4*)))

;; 8 -- signed short
(define-binary-class sshort-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's2* :size value-count))))

;; 9 -- signed long
(define-binary-class slong-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's4* :size value-count))))

;; 10 -- signed rational
(define-binary-class srational ()
  ((numerator s4*)
   (denominator s4*)))

(define-binary-class srational-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'srational :size value-count))))

;; 11
(define-binary-class float-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'f4* :size value-count))))

;; 12
(define-binary-class double-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'f8* :size value-count))))


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

(define-binary-class tiff-fields ()
  ((byte-order tiff-byte-order)
   (magic u2*)
   (ifd-offset tiff-ifd-offset)
   (ifd-list ifd-list)))

(defmethod read-value :around ((type (eql 'tiff)) stream &key)
  (let (*byte-order*)
    (call-next-method)))


(defclass tiff-image ()
  ((length :accessor tiff-image-length :initarg :length)
   (width :accessor tiff-image-width :initarg :width)
   (bits-per-sample :accessor tiff-image-bits-per-sample :initarg :bits-per-sample)
   (samples-per-pixel :accessor tiff-image-samples-per-pixel :initarg :samples-per-pixel)
   (data :accessor tiff-image-data :initarg :data)
   (byte-order :accessor tiff-image-byte-order :initarg :byte-order)))


(defun get-ifd-values (ifd key)
  (let ((field (find key ifd :key 'tag :test '=)))
    (when field
      (data field))))

(defun get-ifd-value (ifd key)
  (let ((values (get-ifd-values ifd key)))
    (when values (elt values 0))))

(defun read-bytes (stream count)
  (let ((buf (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence buf stream)
    buf))

(defun read-grayscale-strip (stream
                             array
                             start-row
                             strip-offset
                             strip-byte-count
                             width
                             compression)
  (file-position stream strip-offset)
  (ecase compression
    (1
     (let ((strip-length (/ strip-byte-count width))
           ;; FIXME: bytes-per-sample will need to change for 1- or
           ;; 4-bit images!
           (bytes-per-pixel 1))
       (loop for i from start-row below (+ start-row strip-length)
          do
            (let ((rowoff (* i width bytes-per-pixel)))
              (loop for j below width
                 do 
                 (setf (aref array (+ rowoff j))
                       (read-byte stream)))))))
    (#.+packbits-compression+
     (error "Not yet!")
     #+nil
     (let ((packed-bits (read-bytes stream strip-byte-count)))
       (let ((decoded (packbits-decode packed-bits))
             (decoded-offset 0))
         (let ((strip-length (/ (length decoded) width samples-per-pixel))
               (bytes-per-sample (/ bytes-per-pixel samples-per-pixel)))
           (loop for i from start-row below (+ start-row strip-length)
              do
              (let ((rowoff (* i width bytes-per-pixel)))
                (loop for j below width
                   do 
                   (let ((pixoff (+ rowoff (* bytes-per-pixel j))))
                     (loop for k below samples-per-pixel
                        for bits across bits-per-sample
                        do 
                        (case bits
                          (8 
                           (setf (aref array (+ pixoff (* k bytes-per-sample)))
                                 (aref decoded decoded-offset))
                           (incf decoded-offset))
                          (16
                           (error "Not yet!"))))))))))))))

(defun read-grayscale-image (stream ifd)
  (let ((image-width (get-ifd-value ifd +image-width-tag+))
        (image-length (get-ifd-value ifd +image-length-tag+))
        (bits-per-sample (or (get-ifd-value ifd +bits-per-sample-tag+) 1))
        (compression (get-ifd-value ifd +compression-tag+))
        (photometric-interpretation (get-ifd-value ifd +photometric-interpretation-tag+))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (rows-per-strip (get-ifd-value ifd +rows-per-strip-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+)))
    (declare (ignore photometric-interpretation))
    (unless (eql bits-per-sample 8)
      (error "I can only read 8-bit grayscale images at the moment."))
    (let* ((bytes-per-pixel 1)
           (data (make-array (* image-width image-length bytes-per-pixel))))
      (loop for strip-offset across strip-offsets
         for strip-byte-count across strip-byte-counts
         for row-offset = 0 then (+ row-offset rows-per-strip)
         do (read-grayscale-strip stream data row-offset
                                  strip-offset strip-byte-count
                                  image-width compression))
      (make-instance 'tiff-image
                     :length image-length :width image-width
                     :bits-per-sample bits-per-sample
                     :samples-per-pixel 1 :data data
                     :byte-order *byte-order*))))

(defun read-rgb-strip (stream array start-row strip-offset
                       strip-byte-count width bits-per-sample samples-per-pixel
                       bytes-per-pixel compression)
  (file-position stream strip-offset)
  (ecase compression
    (#.+no-compression+
     (let ((strip-length (/ strip-byte-count width bytes-per-pixel))
           (bytes-per-sample (/ bytes-per-pixel samples-per-pixel)))
       (loop for i from start-row below (+ start-row strip-length)
          do
            (let ((rowoff (* i width bytes-per-pixel)))
            (loop for j below width
               do 
               (let ((pixoff (+ rowoff (* bytes-per-pixel j))))
                 (loop for k below samples-per-pixel
                    for bits across bits-per-sample
                    do 
                    (case bits
                      (8 
                       (setf (aref array (+ pixoff (* k bytes-per-sample)))
                             (read-byte stream)))
                      (16
                       ;; FIXME! This assumes big-endian data!!!!
                       (setf (aref array (+ pixoff (* k bytes-per-sample)))
                             (read-byte stream)
                             (aref array (+ 1 pixoff (* k bytes-per-sample)))
                             (read-byte stream)))))))))))
    (#.+lzw-compression+
     (let ((lzw (read-bytes stream strip-byte-count)))
       (let ((decoded (lzw-decode lzw))
             (decoded-offset 0))
         (let ((strip-length (/ (length decoded) width samples-per-pixel))
               (bytes-per-sample (/ bytes-per-pixel samples-per-pixel)))
           (loop for i from start-row below (+ start-row strip-length)
              do
              (let ((rowoff (* i width bytes-per-pixel)))
                (loop for j below width
                   do 
                   (let ((pixoff (+ rowoff (* bytes-per-pixel j))))
                     (loop for k below samples-per-pixel
                        for bits across bits-per-sample
                        do 
                        (case bits
                          (8 
                           (setf (aref array (+ pixoff (* k bytes-per-sample)))
                                 (aref decoded decoded-offset))
                           (incf decoded-offset))
                          (16
                           (error "Not yet!"))))))))))))
    (#.+packbits-compression+
     (let ((packed-bits (read-bytes stream strip-byte-count)))
       (let ((decoded (packbits-decode packed-bits))
             (decoded-offset 0))
         (let ((strip-length (/ (length decoded) width samples-per-pixel))
               (bytes-per-sample (/ bytes-per-pixel samples-per-pixel)))
           (loop for i from start-row below (+ start-row strip-length)
              do
              (let ((rowoff (* i width bytes-per-pixel)))
                (loop for j below width
                   do 
                   (let ((pixoff (+ rowoff (* bytes-per-pixel j))))
                     (loop for k below samples-per-pixel
                        for bits across bits-per-sample
                        do 
                        (case bits
                          (8 
                           (setf (aref array (+ pixoff (* k bytes-per-sample)))
                                 (aref decoded decoded-offset))
                           (incf decoded-offset))
                          (16
                           (error "Not yet!"))))))))))))))

(defun read-rgb-image (stream ifd)
  (let ((image-width (get-ifd-value ifd +image-width-tag+))
        (image-length (get-ifd-value ifd +image-length-tag+))
        (samples-per-pixel (get-ifd-value ifd +samples-per-pixel-tag+))
        (bits-per-sample (get-ifd-values ifd +bits-per-sample-tag+))
        (rows-per-strip (get-ifd-value ifd +rows-per-strip-tag+))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+))
        (compression (get-ifd-value ifd +compression-tag+))
        (planar-configuration (get-ifd-value ifd +planar-configuration-tag+))
        (predictor (get-ifd-value ifd +predictor-tag+)))
    (declare (ignore planar-configuration))
    ;; FIXME
    ;; 1. we need to support predictors for lzw encoded images.
    ;; 2. Presumably we'll want planar images as well at some point.
    (let* ((bytes-per-pixel
            (* samples-per-pixel
               (1+ (ash (1- (apply #'max
                                   (map 'list #'identity
                                        bits-per-sample)))
                        -3))))
           (data (make-array (* image-width image-length bytes-per-pixel))))
      (loop for strip-offset across strip-offsets
         for strip-byte-count across strip-byte-counts
         for row-offset = 0 then (+ row-offset rows-per-strip)
         do (read-rgb-strip stream
                            data
                            row-offset
                            strip-offset
                            strip-byte-count
                            image-width
                            bits-per-sample
                            samples-per-pixel
                            bytes-per-pixel
                            compression))
      (case predictor
        (#.+horizontal-differencing+
         (loop for i below image-length
            do 
              (loop for j from 1 below image-width
                 do 
                   (let ((offset (+ (* i image-width samples-per-pixel)
                                    (* samples-per-pixel j))))
                     (loop for k below samples-per-pixel
                        do (setf (aref data (+ offset k))
                                 (logand
                                  (+ (aref data (+ offset k))
                                     (aref data (- (+ offset k) samples-per-pixel)))
                                  #xff))))))))
      (make-instance 'tiff-image
                     :length image-length :width image-width
                     :bits-per-sample bits-per-sample
                     :samples-per-pixel samples-per-pixel
                     :data data :byte-order *byte-order*))))

(defun read-tiff-stream (stream)
  (let* ((fields (read-value 'tiff-fields stream))
         (ifd (entries (first (ifd-list fields)))))
    (let ((photometric-interpretation 
         (get-ifd-value ifd +photometric-interpretation-tag+)))
    (ecase photometric-interpretation
      ((0 1) (read-grayscale-image stream ifd))
      (2 (read-rgb-image stream ifd))))))

(defun read-tiff-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-tiff-stream stream)))

(defun write-tiff-stream (stream obj &key (byte-order :big-endian))
  (declare (ignore stream obj))
  (let ((*byte-order* byte-order))
    ;; FIXME: eventually we'll do something like this:
    ;;   (let ((fields (make-tiff-felds obj))))
    #+nil (write-value 'tiff-fields stream obj)
    (error "not yet implemented")))

(defun write-tiff-file (pathname image)
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-tiff-stream stream image)
    pathname))
