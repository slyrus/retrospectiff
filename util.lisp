;;; file: util.lisp
;;; author: cyrus harmon
;;;
;;; Copyright (c) 2008 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(defun string-contents-of-stream (in)
  "Returns a string with the entire contents of the specified file."
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
           (buffer (make-string buffer-size)))
      (labels ((read-chunks ()
                 (let ((size (read-sequence buffer in)))
                   (if (< size buffer-size)
                       (princ (subseq buffer 0 size) contents)
                       (progn
                         (princ buffer contents)
                         (read-chunks))))))
        (read-chunks)))))

(defun string-contents-of-file (pathname)
  (with-open-file (in pathname :direction :input)
    (string-contents-of-stream in)))

(defun vector-contents-of-stream (in)
  "Returns a string with the entire contents of the specified file."
  (let ((contents (make-array 4096
                              :element-type '(unsigned-byte 8)
                              :fill-pointer 0
                              :adjustable t)))
    (let* ((buffer-size 4096)
           (buffer (make-array buffer-size
                               :element-type '(unsigned-byte 8)
                               :adjustable nil)))
      (labels ((read-chunks ()
                 (let ((size (read-sequence buffer in))
                       (old-fill-pointer (fill-pointer contents)))
                   (ensure-array-size-and-set-fill-pointer
                    contents (+ old-fill-pointer
                                size))
                   (loop for i below size
                      for k from old-fill-pointer
                      do (setf (aref contents k)
                               (aref buffer i)))
                   (unless (< size buffer-size)
                     (read-chunks)))))
        (read-chunks)))
    contents))

(defun vector-contents-of-file (pathname)
  (with-open-file (in pathname 
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (vector-contents-of-stream in)))

(defun ensure-array-size-and-set-fill-pointer (array fill-pointer)
  (let ((length (array-dimension array 0)))
    (when (>= fill-pointer length)
      (adjust-array array (max 256
                               (+ length (ash length -1))
                               fill-pointer)))
    (setf (fill-pointer array) fill-pointer)))

