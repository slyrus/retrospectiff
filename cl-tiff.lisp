
(in-package :tiff)

(defparameter *byte-order* nil)

(defun read-bytes (stream count)
  (let ((buf (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence buf stream)
    buf))

(defun read-int-16 (stream)
  (let ((bytes (read-bytes stream 2)))
    (ecase *byte-order*
      ((:big-endian nil) (+ (ash (aref bytes 0) 8) (aref bytes 1)))
      (:little-endian (+ (ash (aref bytes 1) 8) (aref bytes 0))))))

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

(defun read-tiff-header (stream)
  (let ((byte-order (read-bytes stream 2)))
    ))

(defun read-ifd-entry (stream)
  (let ((tag (read-int-16 stream))
        (field-type (read-int-16 stream))
        (values (read-int-32 stream))
        (value-offset (read-int-32 stream)))
    (list tag field-type values value-offset)))

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
      ifds)))

(defun read-tiff-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type :default)
    (read-tiff-stream stream)))