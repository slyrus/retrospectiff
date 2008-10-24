
(in-package :tiff)

(defparameter *byte-order* nil)

(defun read-bytes (stream count)
  (let ((buf (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence buf stream)
    buf))

(defun read-int-16 (stream &key (byte-order *byte-order*))
  (let ((bytes (read-bytes stream 2)))
    (ecase byte-order
      ((:big-endian nil) (+ (ash (aref bytes 0) 8) (aref bytes 1)))
      (:little-endian (+ (ash (aref bytes 1) 8) (aref bytes 0))))))

(defun read-int-16-array (stream count)
  (let ((array (make-array count :element-type '(unsigned-byte 16))))
    (loop for i below count
       do (setf (aref array i) (read-int-16 stream)))
    array))

(defun read-int-32 (stream)
  (let ((bytes (read-bytes stream 4)))
    (ecase *byte-order*
      ((:big-endian nil) (+ (ash (aref bytes 0) 24)
                            (ash (aref bytes 1) 16)
                            (ash (aref bytes 2) 8)
                            (aref bytes 3)))
      (:little-endian (+ (ash (aref bytes 3) 24)
                         (ash (aref bytes 2) 16)
                         (ash (aref bytes 1) 8)
                         (aref bytes 0))))))

(defun read-int-32-array (stream count)
  (let ((array (make-array count :element-type '(unsigned-byte 32))))
    (loop for i below count
       do (setf (aref array i) (read-int-32 stream)))
    array))

(defun read-rational (stream)
  (/ (read-int-32 stream) (read-int-32 stream)))

(defun read-rational-array (stream count)
  (let ((array (make-array count :element-type '(unsigned-byte 32))))
    (loop for i below count
       do (setf (aref array i) (read-rational stream)))
    array))

(defvar *tiff-field-types* nil)

(defclass tiff-field ()
  ((reader :accessor tiff-field-reader :initarg reader)))

(defun read-field-bytes (stream count)
  (if (<= count 4)
      (loop for i below 4
         for byte = (read-byte stream)
         when (< i count)
         collect byte)
      (let ((offset (read-int-32 stream))
            (field-pos (file-position stream)))
        (file-position stream offset)
        (prog1
            (read-bytes stream count)
          (file-position stream field-pos)))))

(defun split-strings (char-list)
  (let ((strings))
    (cond ((listp char-list)
           (loop for char in char-list with string
              do
                (if (zerop char)
                    (push (coerce (nreverse string) 'string) strings)
                    (push (code-char char) string))))
          ((vectorp char-list)
           (loop for char across char-list with string
              do
                (if (zerop char)
                    (push (coerce (nreverse string) 'string) strings)
                    (push (code-char char) string)))))
    strings))

(defun read-field-asciis (stream count)
  (if (<= count 4)
      (let ((strs (loop for i below 4
                     for byte = (read-byte stream)
                     when (< i count)
                     collect byte)))
        (split-strings strs))
      (let ((offset (read-int-32 stream))
            (field-pos (file-position stream)))
        (file-position stream offset)
        (prog1
            (split-strings (read-bytes stream count))
          (file-position stream field-pos)))))

(defun read-field-shorts (stream count)
  (if (<= count 2)
      (loop for i below 2
         for short = (read-int-16 stream)
         when (< i count)
         collect short)
      (let ((offset (read-int-32 stream))
            (field-pos (file-position stream)))
        (file-position stream offset)
        (prog1
            (read-int-16-array stream count)
          (file-position stream field-pos)))))

(defun read-field-longs (stream count)
  (let ((value-or-offset (read-int-32 stream)))
    (if (<= count 1)
        value-or-offset
        (let ((field-pos (file-position stream)))
          (file-position stream value-or-offset)
          (prog1
              (read-int-32-array stream count)
            (file-position stream field-pos))))))

(defun read-field-rationals (stream count)
    (let ((offset (read-int-32 stream)))
      (let ((field-pos (file-position stream)))
        (file-position stream offset)
        (prog1
            (read-rational-array stream count)
          (file-position stream field-pos)))))

(defconstant +image-width-tag+ 256)
(defconstant +image-length-tag+ 257)
(defconstant +bits-per-sample-tag+ 258)
(defconstant +compression-tag+ 259)
(defconstant +photometric-interpretation-tag+ 262)
(defconstant +strip-offsets-tag+ 273)
(defconstant +samples-per-pixel-tag+ 277)
(defconstant +rows-per-strip-tag+ 278)
(defconstant +rows-per-strip-tag+ 278)
(defconstant +strip-byte-counts-tag+ 279)
(defconstant +x-resolution-tag+ 282)
(defconstant +y-resolution-tag+ 283)
(defconstant +resolution-unit-tag+ 296)

(macrolet ((def-tiff-field-type (type value reader)
             `(progn
                (defconstant ,(intern (format nil "+field-type-~A+" type)) ,value)
                (pushnew (cons ',type ,value) *tiff-field-types* :test 'equal))))
  (def-tiff-field-type byte 1 'read-field-bytes)
  (def-tiff-field-type ascii 2 nil)
  (def-tiff-field-type short 3 read-field-shorts)
  (def-tiff-field-type long 4 nil)
  (def-tiff-field-type rational 5 nil)
  (def-tiff-field-type sbyte 6 nil)
  (def-tiff-field-type undefined 7 nil)
  (def-tiff-field-type sshort 8 nil)
  (def-tiff-field-type slong 9 nil)
  (def-tiff-field-type srational 10 nil)
  (def-tiff-field-type float 11 nil)
  (def-tiff-field-type double 12 nil))

(defun read-ifd-tag-type-and-count (stream)
  (let ((tag (read-int-16 stream))
        (field-type (read-int-16 stream))
        (value-count (read-int-32 stream)))
    (list tag field-type value-count)))

(defun read-ifd-entry (stream)
  (let ((tag-type-and-count (read-ifd-tag-type-and-count stream)))
    (destructuring-bind (tag field-type value-count)
        tag-type-and-count
      (list tag field-type value-count
            (cond ((= field-type +field-type-short+)
                   (read-field-shorts stream value-count))
                  ((= field-type +field-type-ascii+)
                   (read-field-asciis stream value-count))
                  ((= field-type +field-type-long+)
                   (read-field-longs stream value-count))
                  ((= field-type +field-type-rational+)
                   (read-field-rationals stream value-count))
                  ((= field-type +field-type-undefined+)
                   (read-field-bytes stream value-count))
                  (t (let ((value-offset (read-int-32 stream)))
                       value-offset)))))))

(defun ifd-entry-values (ifd-entry)
  (elt ifd-entry 3))

(defun read-ifd (stream)
  (let ((ifd-count (read-int-16 stream)))
    (loop for i below ifd-count
       collect (read-ifd-entry stream))))

(defun read-ifds (stream)
  (loop for ifd = (read-int-32 stream)
     while (not (zerop ifd))
     collect
       (progn
         (file-position stream ifd)
         (read-ifd stream))))

(defun get-ifd-values (ifd key)
  (let ((field (find key ifd :key 'car :test '=)))
    (ifd-entry-values field)))

(defun read-grayscale-image (stream ifd photometric-interpretation)
  (declare (ignore ifd photometric-interpretation))
  (error "Not yet!"))

(defun read-strip (stream
                   array
                   start-row
                   strip-offset
                   strip-byte-count
                   width
                   bits-per-sample
                   samples-per-pixel
                   bytes-per-pixel)
  (let ((strip-length (/ strip-byte-count width samples-per-pixel))
        (bytes-per-sample (/ bytes-per-pixel samples-per-pixel)))
    (file-position stream strip-offset)
    (print (cons start-row strip-length))
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
                          (error "Not yet!"))))))))))

(defun read-rgb-image (stream ifd)
  (let ((image-width (car (get-ifd-values ifd +image-width-tag+)))
        (image-length (car (get-ifd-values ifd +image-length-tag+)))
        (samples-per-pixel (car (get-ifd-values ifd +samples-per-pixel-tag+)))
        (bits-per-sample (get-ifd-values ifd +bits-per-sample-tag+))
        (rows-per-strip (car (get-ifd-values ifd +rows-per-strip-tag+)))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+)))
    (let* ((bytes-per-pixel
            (* samples-per-pixel (1+ (ash (1- (apply #'max (map 'list #'identity bits-per-sample))) -3))))
           (image (make-array (* image-width image-length bytes-per-pixel))))
      (loop for strip-offset across strip-offsets
         for strip-byte-count across strip-byte-counts
         for row-offset = 0 then (+ row-offset rows-per-strip)
         do (read-strip stream
                        image
                        row-offset
                        strip-offset
                        strip-byte-count
                        image-width
                        bits-per-sample
                        samples-per-pixel
                        bytes-per-pixel))
      (list image-length image-width samples-per-pixel image))))

(defun read-image (stream ifd)
  (let ((photometric-interpretation (car (get-ifd-values ifd +photometric-interpretation-tag+))))
    (ecase photometric-interpretation
      ((0 1) (read-grayscale-image stream ifd photometric-interpretation))
      (2 (read-rgb-image stream ifd)))))

(defun read-tiff-stream (stream)
  (let ((*byte-order*))
    (let ((byte-order (read-int-16 stream)))
      (ecase byte-order
        (#x4949 (setf *byte-order* :little-endian))
        (#x4d4d (setf *byte-order* :big-endian))))
    (let ((tiff-magic (read-int-16 stream)))
      (unless (= tiff-magic 42)
        (error "Error reading TIFF file. ~S is not 42." tiff-magic)))
    (let ((ifds (read-ifds stream)))
      (let ((ifd (car ifds)))
        (read-image stream ifd)))))

(defun read-tiff-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type :default)
    (read-tiff-stream stream)))

(defun write-tiff-stream (stream image)
  (declare (ignore stream image))
  (error "Not yet!"))

(defun write-tiff-file (pathname image)
  (with-open-file (stream pathname :direction :output :element-type :default)
    (write-tiff-stream stream image)))
