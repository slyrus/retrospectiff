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
