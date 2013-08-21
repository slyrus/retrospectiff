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

(defparameter *byte-order* :big-endian)
(defvar *tiff-file-offset*)

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
              (loop for low-bit to (* bits-per-byte (1- bytes)) by bits-per-byte
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
                   (let ((cur (file-position out))
                         (offset *tiff-file-offset*))
                     (progn (file-position out offset)
                            (loop for x across value
                               do (write-value type out x))
                            ;; need to make sure this is word aligned!
                            (setf *tiff-file-offset*
                                  (ash (ash (1+ (file-position out)) -1) 1))
                            (file-position out cur)
                            (write-value 'u4* out offset)))
                   (progn (loop for x across value
                             do (write-value type out x))
                          (loop for i below pad
                             do (write-value type out 0))))))))


(defgeneric bytes-per-entry (class))
(defgeneric entry-bytes (ifd-entry))

(defmethod entry-bytes ((entry ifd-entry))
  (* (value-count entry)
     (bytes-per-entry (class-of entry))))

;; 1 - byte
(define-binary-class byte-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u1 :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'byte-ifd-entry)))) 1)

;; 2 - ascii
(define-binary-class ascii-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'iso-8859-1-char :size value-count :element-type 'character))))
(defmethod bytes-per-entry ((class (eql (find-class 'ascii-ifd-entry)))) 1)

;; 3 -- short
(define-binary-class short-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u2* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'short-ifd-entry)))) 2)

;; 4 -- long
(define-binary-class long-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u4* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'long-ifd-entry)))) 4)

;; 5 -- rational
(define-binary-class rational ()
  ((numerator u4*)
   (denominator u4*)))

(define-binary-class rational-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'rational :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'rational-ifd-entry)))) 8)

;; 6 -- signed byte
(define-binary-class sbyte-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's1* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'sbyte-ifd-entry)))) 8)

;; 7 -- undefined (and unused, at least for now)
#+nil
(define-binary-class undefined-ifd-entry (ifd-entry)
  ((value-offset u4*)))
#+nil
(defmethod bytes-per-entry ((class (eql (find-class 'undefined-ifd-entry)))) 4)

;; 8 -- signed short
(define-binary-class sshort-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's2* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'sshort-ifd-entry)))) 2)

;; 9 -- signed long
(define-binary-class slong-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's4* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'slong-ifd-entry)))) 4)

;; 10 -- signed rational
(define-binary-class srational ()
  ((numerator s4*)
   (denominator s4*)))

(define-binary-class srational-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'srational :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'srational-ifd-entry)))) 8)

;; 11
(define-binary-class float-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'f4* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'float-ifd-entry)))) 4)

;; 12
(define-binary-class double-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'f8* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'double-ifd-entry)))) 8)


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
              do
                (setf (entries ifd)
                      (sort (entries ifd) #'< :key #'tag))
                (write-value 'ifd out ifd))))

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
   (samples-per-pixel :accessor tiff-image-samples-per-pixel
                      :initarg :samples-per-pixel
                      :initform nil)
   (data :accessor tiff-image-data :initarg :data)
   (byte-order :accessor tiff-image-byte-order :initarg :byte-order)
   (color-map :accessor tiff-image-color-map :initarg :color-map :initform nil)
   (min-is-white :accessor tiff-image-min-is-white :initarg :min-is-white
                 :initform nil)))


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

(defvar *compressions*
  (list (list +no-compression+ #'identity #'identity)
	(list +packbits-compression+ #'packbits-decode #'packbits-encode)
	(list +lzw-compression+ #'lzw-decode #'lzw-encode)))

(defun find-compression-decoder (compression)
  (let ((decoder (cadr (assoc compression *compressions*))))
    (if decoder
	decoder
	(error "Compression not supported: ~a" compression))))

(defun read-grayscale-strip (stream
                             array
                             start-row
                             strip-offset
                             strip-byte-count
                             width
                             bits-per-sample
                             compression)
  (file-position stream strip-offset)
  (ecase compression
    (#.+no-compression+
     (ecase bits-per-sample
       (1
        (let* ((bytes-per-row (1+ (ash (1- width) -3)))
               (strip-length (/ strip-byte-count bytes-per-row)))
          (loop for i from start-row below (+ start-row strip-length)
             do
               (let ((rowoff (* i bytes-per-row)))
                 (read-sequence array stream
                                :start rowoff
                                :end (+ rowoff bytes-per-row))))))
       (8
        (let ((strip-length (/ strip-byte-count width))
              (bytes-per-pixel 1))
          (loop for i from start-row below (+ start-row strip-length)
             do
             (let ((rowoff (* i width bytes-per-pixel)))
               (read-sequence array stream
                              :start rowoff
                              :end (+ rowoff width))))))))
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
    (case bits-per-sample
      (1
       (let* ((bytes-per-row (1+ (ash (1- image-width) -3)))
              (data (make-array (* bytes-per-row image-length))))
         (loop for strip-offset across strip-offsets
            for strip-byte-count across strip-byte-counts
            for row-offset = 0 then (+ row-offset rows-per-strip)
            do (read-grayscale-strip stream data row-offset
                                     strip-offset strip-byte-count
                                     image-width
                                     bits-per-sample
                                     compression))
         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel 1 :data data
                        :byte-order *byte-order*
                        :min-is-white (= photometric-interpretation
                                         +photometric-interpretation-white-is-zero+))))
      (8
       (let* ((bytes-per-pixel 1)
              (data (make-array (* image-width image-length bytes-per-pixel))))
         (loop for strip-offset across strip-offsets
            for strip-byte-count across strip-byte-counts
            for row-offset = 0 then (+ row-offset rows-per-strip)
            do (read-grayscale-strip stream data row-offset
                                     strip-offset strip-byte-count
                                     image-width
                                     bits-per-sample
                                     compression))
         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel 1 :data data
                        :byte-order *byte-order*)))
      (t
       (unless (eql bits-per-sample 8)
         (error "I can only read 8-bit grayscale images at the moment."))))))

(defun read-rgb-strip (stream array start-row strip-offset
                       strip-byte-count width bits-per-sample samples-per-pixel
                       bytes-per-pixel compression)
  (file-position stream strip-offset)
  (let ((compressed (read-bytes stream strip-byte-count)))
    (let ((decoded (funcall (find-compression-decoder compression) compressed))
	  (decoded-offset 0))
      (let ((strip-length (/ (length decoded) width bytes-per-pixel))
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
			(let ((ordering (case *byte-order*
					  (:big-endian '(0 1))
					  (:little-endian '(1 0)))))
			  (setf (aref array (+ (first ordering) pixoff (* k bytes-per-sample)))
				(aref decoded decoded-offset)
				(aref array (+ (second ordering) pixoff (* k bytes-per-sample)))
				(aref decoded (1+ decoded-offset)))
			  (incf decoded-offset 2)))))))))))))

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

(defun read-indexed-strip (stream array start-row strip-offset
                           strip-byte-count width bits-per-sample
                           bytes-per-pixel compression)
  (file-position stream strip-offset)
  (let ((compressed (read-bytes stream strip-byte-count)))
    (let ((decoded (funcall (find-compression-decoder compression) compressed))
	  (decoded-offset 0))
      (let ((strip-length (/ (length decoded) width)))
	(loop for i from start-row below (+ start-row strip-length)
	   do
	   (let ((rowoff (* i width bytes-per-pixel)))
	     (loop for j below width
		do
		(let ((pixoff (+ rowoff (* bytes-per-pixel j))))
		  (case bits-per-sample
		    (8
		     (setf (aref array pixoff)
			   (aref decoded decoded-offset))
		     (incf decoded-offset))
		    (16
		     (error "Not yet!")))))))))))

(defun read-indexed-image (stream ifd)
  (let ((image-width (get-ifd-value ifd +image-width-tag+))
        (image-length (get-ifd-value ifd +image-length-tag+))
        (bits-per-sample (get-ifd-value ifd +bits-per-sample-tag+))
        (rows-per-strip (get-ifd-value ifd +rows-per-strip-tag+))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+))
        (compression (get-ifd-value ifd +compression-tag+))
        (predictor (get-ifd-value ifd +predictor-tag+))
        (color-map (get-ifd-values ifd +color-map-tag+)))
    (let* ((k (expt 2 bits-per-sample))
           (color-index (make-array k)))
      (loop for i below k
         do (setf (aref color-index i)
                  (list (aref color-map i)
                        (aref color-map (+ k i))
                        (aref color-map (+ (ash k 1) i)))))
      ;; FIXME
      ;; 1. we need to support predictors for lzw encoded images.
      (let* ((bytes-per-pixel
              (1+ (ash (1- bits-per-sample)
                       -3)))
             (data (make-array (* image-width image-length bytes-per-pixel))))
        (loop for strip-offset across strip-offsets
           for strip-byte-count across strip-byte-counts
           for row-offset = 0 then (+ row-offset rows-per-strip)
           do (read-indexed-strip stream
                                  data
                                  row-offset
                                  strip-offset
                                  strip-byte-count
                                  image-width
                                  bits-per-sample
                                  bytes-per-pixel
                                  compression))
        (case predictor
          (#.+horizontal-differencing+
           (loop for i below image-length
              do 
              (loop for j from 1 below image-width
                 do 
                 (let ((offset (+ (* i image-width) j)))
                   (setf (aref data offset)
                         (logand
                          (+ (aref data offset)
                             (aref data (1- offset)))
                          #xff)))))))
        (make-instance 'tiff-image
                       :length image-length :width image-width
                       :bits-per-sample bits-per-sample
                       :data data :byte-order *byte-order*
                       :color-map color-index)))))

(defun read-tiff-stream (stream)
  (let* ((fields (read-value 'tiff-fields stream))
         (ifd (entries (first (ifd-list fields)))))
    (let ((photometric-interpretation 
         (get-ifd-value ifd +photometric-interpretation-tag+)))
    (ecase photometric-interpretation
      (#.+photometric-interpretation-white-is-zero+
       (read-grayscale-image stream ifd))
      (#.+photometric-interpretation-black-is-zero+
       ;;; FIXME! This image should be inverted
       (read-grayscale-image stream ifd))
      (#.+photometric-interpretation-rgb+
       (read-rgb-image stream ifd))
      (#.+photometric-interpretation-palette-color+
       (read-indexed-image stream ifd))))))

(defun read-tiff-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-tiff-stream stream)))

(defun add-ifd-entry (ifd entry)
  (push entry (entries ifd))
  (incf (entry-count ifd))
  ifd)

(defun vectorize (data)
  (etypecase data
    (vector data)
    (list (apply #'vector data))
    (nil nil)
    (atom (vector data))))

(defun make-ifd-entry-short (tag data)
  (let ((data (vectorize data)))
    (make-instance 'short-ifd-entry
                   :tag tag
                   :field-type +field-type-short+
                   :data data
                   :value-count (length data))))

(defun make-ifd-entry-long (tag data)
  (let ((data (vectorize data)))
    (make-instance 'long-ifd-entry
                   :tag tag
                   :field-type +field-type-long+
                   :data data
                   :value-count (length data))))

(defun make-ifd-entry-rational (tag data)
  (let ((data (vectorize data)))
    (make-instance 'rational-ifd-entry
                   :tag tag
                   :field-type +field-type-rational+
                   :data (map 'vector
                              (lambda (x)
                                (make-instance 'rational :numerator (car x)
                                               :denominator (cdr x)))
                              data)
                   :value-count (length data))))


(defun ifd-entry-out-of-line-bytes (entry)
  (let ((bytes (entry-bytes entry)))
    (if (> bytes 4) bytes 0)))

;; we should return the number of strips (and possibly the length of
;; each strip (uncompressed), but not yet)..
(defun compute-rows-per-strip (image-length
                               bytes-per-row
                               &key (strip-size #x40000))
  (let ((strip-rows (truncate strip-size bytes-per-row)))
    (min image-length strip-rows)))

(defun make-tiff-fields (image)
  (with-accessors
        ((image-width tiff-image-width)
         (image-length tiff-image-length)
         (image-data tiff-image-data)
         (bits-per-sample tiff-image-bits-per-sample)
         (samples-per-pixel tiff-image-samples-per-pixel))
      image
    (let* ((num-bits-per-sample (if (typep bits-per-sample 'sequence)
                                    (elt bits-per-sample 0)
                                    bits-per-sample))
           (bytes-per-pixel (* samples-per-pixel (ash num-bits-per-sample -3)))
           (bytes-per-row (* image-width bytes-per-pixel))
           (rows-per-strip (compute-rows-per-strip image-length bytes-per-row))
           (fields (make-instance 'tiff-fields
                                  :byte-order *byte-order*
                                  :magic 42
                                  :ifd-list nil))
           (ifd (make-instance 'ifd
                               :entry-count 0
                               :entries nil
                               :next-ifd-offset 0)))

      (destructuring-bind (strip-offsets strip-byte-counts)
          (apply #'mapcar #'list
                 (loop for i below image-length by rows-per-strip
                    for byte-offset from i by (* rows-per-strip
                                                 bytes-per-row) 
                    collect (list byte-offset 
                                  (* bytes-per-row
                                     (- (min (+ i rows-per-strip)
                                             image-length) i)))))
        (reduce #'add-ifd-entry 
                (list (make-ifd-entry-long +image-length-tag+ image-length)
                      (make-ifd-entry-long +image-width-tag+ image-width)
                      (make-ifd-entry-short +bits-per-sample-tag+ bits-per-sample)
                      (make-ifd-entry-short +samples-per-pixel-tag+ samples-per-pixel)
                      (make-ifd-entry-rational +x-resolution-tag+ (vector (cons 72 1)))
                      (make-ifd-entry-rational +y-resolution-tag+ (vector (cons 72 1)))
                      (make-ifd-entry-short +resolution-unit-tag+ 2))
                :initial-value ifd)
        (cond
          ((= samples-per-pixel 1)
           (add-ifd-entry 
            ifd
            (make-ifd-entry-short +photometric-interpretation-tag+
                                  +photometric-interpretation-black-is-zero+)))
          ((member samples-per-pixel '(3 4))
           (add-ifd-entry 
            ifd
            (make-ifd-entry-short +photometric-interpretation-tag+
                                  +photometric-interpretation-rgb+))))

        (add-ifd-entry ifd
                       (make-ifd-entry-long +rows-per-strip-tag+ rows-per-strip))
        (add-ifd-entry ifd
                       (make-ifd-entry-long +strip-byte-counts-tag+ strip-byte-counts))
        
        (setf (ifd-list fields) (list ifd))

        (incf *tiff-file-offset* 8)
        (setf (ifd-offset fields) *tiff-file-offset*)
        
        (let ((num-entries (entry-count ifd)))
          (incf *tiff-file-offset* (+ 2 (* num-entries 12))))
        
        (let ((out-of-line-data-size 
               (* 4 (length strip-offsets))))
          (loop for entry in (entries ifd)
             do (incf out-of-line-data-size 
                      (ifd-entry-out-of-line-bytes entry)))
        
          ;; skip one more ifd-entry
          (incf *tiff-file-offset* 12)
          (incf *tiff-file-offset* 4)
                    
          ;; *file-offset* to the strip-offsets
          (add-ifd-entry
           ifd
           (make-ifd-entry-long
            +strip-offsets-tag+
            (map 'vector
                 (lambda (x) (+ x
                                *tiff-file-offset*
                                out-of-line-data-size))
                 strip-offsets)))

          (values fields out-of-line-data-size strip-offsets strip-byte-counts))))))

;;;
;;; The general strategy here is to:
;;;
;;; 1. make the TIFF Image File Directory (we're only going to deal
;;; with single images per TIFF file for the moment)
;;;
;;; 2. Compute the offsets of the first (and only IFD -- probably 8)
;;;
;;; 3. Compute the offset of the various IFD arrays that aren't
;;;    represented inline -- starting at the offset of the IFD + (2 +
;;;    number of directory entries * 12)
;;; 
;;; 4. Compute the offset of the strip/sample data
;;;
;;; 5. Write the TIFF Header
;;;
;;; 6. Write the IFD directory entries (inline portions), then write
;;; the non-inline values
;;;
;;; 7. Write the sample (strip) data
(defun write-tiff-stream (stream obj &key byte-order)
  (let ((*byte-order* (or byte-order *byte-order*))
        (*tiff-file-offset* 0))
    (multiple-value-bind (fields out-of-line-data-size strip-offsets strip-byte-counts) 
        (make-tiff-fields obj)
      (write-value 'tiff-fields stream fields)
      (file-position stream (+ (file-position stream) out-of-line-data-size))
      (with-accessors
            ((image-width tiff-image-width)
             (image-length tiff-image-length)
             (image-data tiff-image-data))
          obj
        (loop for start in strip-offsets
           for count in strip-byte-counts
           do
             (write-sequence (subseq image-data start
                                     (+ start count))
                             stream))))))

(defun write-tiff-file (pathname image &rest args &key (if-exists :error) &allow-other-keys)
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists)
    (apply #'write-tiff-stream stream image (remove-keyword-args :if-exists args))
    pathname))

