;;; file: package.lisp
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

(defun packbits-encode (raw-vector)
  (typecase raw-vector
    (string
     (packbits-encode
      (map 'vector #'char-code raw-vector)))
    (vector
     (let ((output (make-array 256
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0
                               :adjustable t)))
       #+nil
       (loop for i below (length vector)
            )
       output))))

(defun packbits-decode (compressed-vector &key stream)
  (let ((output (make-array 256
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0
                            :adjustable t))
        (input-byte-offset 0)
        (input-length (length compressed-vector))
        (output-byte-offset 0))
    (flet ((output-byte (byte)
             (if stream
                 (write-byte byte stream)
                 (progn
                   (ensure-array-size-and-set-fill-pointer
                    output output-byte-offset)
                   (setf (aref output output-byte-offset) byte)
                   (setf (fill-pointer output)
                         (incf output-byte-offset)))))
           (next-input-byte ()
             (when (< input-byte-offset input-length)
               (prog1
                   (aref compressed-vector input-byte-offset)
                 (incf input-byte-offset)))))
      (loop for byte = (next-input-byte)
         while byte
         do 
           (print byte)
           (cond ((equal -128 byte))
                 ((minusp byte)
                  (let ((output (next-input-byte)))
                    (loop for i to (- byte)
                       do
                       (output-byte output))))
                 (t
                  (loop for i to byte
                     do
                     (output-byte (next-input-byte)))))))
    output))
