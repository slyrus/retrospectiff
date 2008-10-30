
(in-package :retrospectiff)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
  (defconstant +planar-configuration-tag+ 284)
  (defconstant +resolution-unit-tag+ 296)
  (defconstant +predictor-tag+ 317)
  
  (defconstant +horizontal-differencing+ 2)

  (defconstant +packbits-compression+ #x8005)
  (defconstant +lzw-compression+ 5)

  (defconstant +field-type-byte+ 1)
  (defconstant +field-type-ascii+ 2)
  (defconstant +field-type-short+ 3)
  (defconstant +field-type-long+ 4)
  (defconstant +field-type-rational+ 5)
  (defconstant +field-type-sbyte+ 6)
  (defconstant +field-type-undefined+ 7)
  (defconstant +field-type-sshort+ 8)
  (defconstant +field-type-slon+ 9)
  (defconstant +field-type-srationa+ 10)
  (defconstant +field-type-float+ 11)
  (defconstant +field-type-double+ 12))

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

(defun write-int-16 (stream int &key (byte-order *byte-order*))
  (ecase byte-order
    ((:big-endian nil)
     (write-byte (ldb (byte 8 8) int) stream)
     (write-byte (ldb (byte 8 0) int) stream))
    (:little-endian
     (write-byte (ldb (byte 8 0) int) stream)
     (write-byte (ldb (byte 8 8) int) stream))))

(defun read-int-16-array (stream count)
  (let ((array (make-array count :element-type '(unsigned-byte 16))))
    (loop for i below count
       do (setf (aref array i) (read-int-16 stream)))
    array))

(defun write-int-16-array (stream array)
  (dotimes (i (length array))
    (write-int-16 stream (aref array i))))

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

(defun write-int-32 (stream int &key (byte-order *byte-order*))
  (ecase byte-order
    ((:big-endian nil)
     (write-byte (ldb (byte 8 24) int) stream)
     (write-byte (ldb (byte 8 16) int) stream)
     (write-byte (ldb (byte 8 8) int) stream)
     (write-byte (ldb (byte 8 0) int) stream))
    (:little-endian
     (write-byte (ldb (byte 8 0) int) stream)
     (write-byte (ldb (byte 8 8) int) stream)
     (write-byte (ldb (byte 8 16) int) stream)
     (write-byte (ldb (byte 8 24) int) stream))))

(defun read-int-32-array (stream count)
  (let ((array (make-array count :element-type '(unsigned-byte 32))))
    (loop for i below count
       do (setf (aref array i) (read-int-32 stream)))
    array))

(defun write-int-32-array (stream array)
  (dotimes (i (length array))
    (write-int-32 stream (aref array i))))

(defun read-rational (stream)
  (/ (read-int-32 stream) (read-int-32 stream)))

(defun write-rational (stream)
  (declare (ignore stream))
  (error "not yet!"))

(defun read-rational-array (stream count)
  (let ((array (make-array count :element-type '(unsigned-byte 32))))
    (loop for i below count
       do (setf (aref array i) (read-rational stream)))
    array))

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
    (when field
      (ifd-entry-values field))))

(defun get-ifd-value (ifd key)
  (let ((values (get-ifd-values ifd key)))
    (when values (car values))))

(defun read-grayscale-image (stream ifd photometric-interpretation)
  (declare (ignore stream ifd photometric-interpretation))
  (error "Not yet!"))

(defun read-rgb-strip (stream
                       array
                       start-row
                       strip-offset
                       strip-byte-count
                       width
                       bits-per-sample
                       samples-per-pixel
                       bytes-per-pixel
                       compression)
  (declare (optimize (debug 2)))
  (file-position stream strip-offset)
  (ecase compression
    (1
     (let ((strip-length (/ strip-byte-count width samples-per-pixel))
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
                       (error "Not yet!"))))))))))
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
    (+packbits-compression+
     (error "Not yet!"))))

(defclass tiff-image ()
  ((length :accessor tiff-image-length :initarg :length)
   (width :accessor tiff-image-width :initarg :width)
   (samples-per-pixel :accessor tiff-image-samples-per-pixel :initarg :samples-per-pixel)
   (data :accessor tiff-image-data :initarg :data)))

(defun read-rgb-image (stream ifd)
  (declare (optimize (debug 2)))
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
    ;; 1. we need to support predictorsfor lzw encoded images.
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
                     :length image-length
                     :width image-width
                     :samples-per-pixel samples-per-pixel
                     :data data))))

(defun read-image (stream ifd)
  (let ((photometric-interpretation
         (get-ifd-value ifd +photometric-interpretation-tag+)))
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
