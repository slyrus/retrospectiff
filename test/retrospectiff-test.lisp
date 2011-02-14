;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package #:retrospectiff-test)

;;; TIFF reading

(defparameter *snow-image* (read-tiff-file "test/images/snow.tiff"))
(defparameter *snow-lzw-image* (read-tiff-file "test/images/snow-lzw.tiff"))

(assert (equalp (tiff-image-data *snow-image*)
                (tiff-image-data *snow-lzw-image*)))

#+nil (write-tiff-file "foo.tiff" *snow-image* :if-exists :supersede)

#+nil
(with-open-file (stream "test/images/snow.tiff" :element-type '(unsigned-byte 8))
  (let ((length (file-length stream)))
    (let ((vector (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence vector stream)
      (with-open-file (outstream "quux.tiff"
                                 :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
        (write-sequence vector outstream)))))

