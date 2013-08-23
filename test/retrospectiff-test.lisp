;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package #:retrospectiff-test)

(defun %file (name) (asdf:system-relative-pathname "retrospectiff" (format nil "test/images/~a.tiff" name)))
(defun %image (name) (read-tiff-file (%file name)))
(defun %data (name) (tiff-image-data (%image name)))


(defsuite* compressions ())

(deftest compare-grayscale ()
  (is (equalp (%data "snow-grayscale") (%data "snow-grayscale-lzw"))))

(deftest compare-indexed ()
  (is (equalp (%data "snow-indexed") (%data "snow-indexed-lzw"))))

(deftest compare-rgb ()
  (is (equalp (%data "snow-rgb-packbits") (%data "snow-rgb-lzw")))
  (is (equalp (%data "snow-rgb-lzw") (%data "snow-rgb"))))


(defun r/w/r? (name)
  (let ((image (%image name)))
    (equalp (tiff-image-data image)
	    (tiff-image-data
	     (with-input-from-sequence (in (with-output-to-sequence (out)
					     (write-tiff-stream out image)))
	       (read-tiff-stream in))))))

(defsuite* read/write/read ())

(deftest r/w/r-grayscale ()
  (loop for name in '("snow-grayscale" "snow-grayscale-lzw")
       do (is (r/w/r? name))))

(deftest r/w/r-indexed ()
  (loop for name in '("snow-indexed" "snow-indexed-lzw")
       do (is (r/w/r? name))))

(deftest r/w/r-rgb ()
  (loop for name in '("snow-rgb" "snow-rgb-packbits" "snow-rgb-lzw")
       do (is (r/w/r? name))))
