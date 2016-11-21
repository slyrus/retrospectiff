;;; file: package.lisp
;;; author: cyrus harmon
;;;
;;; Copyright (c) 20016 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(defun jpeg-encode (raw-vector)
  (let ((stream (flexi-streams:make-in-memory-input-stream raw-vector)))
    (declare (ignore stream))
    (error "JPEG encoding not supported yet!")))

(defun read-jpeg-tables (jpeg-image-info)
  (let ((jpeg-tables-stream (flexi-streams:make-in-memory-input-stream
                             (jpeg-tables jpeg-image-info))))
    (let ((image (jpeg::make-descriptor)))
      (unless (= (jpeg::read-marker jpeg-tables-stream) jpeg::+M_SOI+)
        (error "Unrecognized JPEG format"))
      (let ((marker (jpeg::interpret-markers image 0 jpeg-tables-stream)))
        (unless (= marker jpeg::+M_EOI+)
          (error "Unrecognized JPEG format")))
      (setf (jpeg-image jpeg-image-info) image))))

(defun jpeg-decode (compressed-vector jpeg-image-info)
  ;;  NOTE: we currently read the jpeg-tables every time through. We
  ;;  should be able to cache this, but we don't as its state gets
  ;;  modified by the JPEG reading stuff and we can't just reuse it
  ;;  each time through. So..., for the moment at least, we make a new
  ;;  one each time through.
  (read-jpeg-tables jpeg-image-info)
  (let ((image (jpeg-image jpeg-image-info)))
    (let ((stream (flexi-streams:make-in-memory-input-stream compressed-vector)))
      (unless (= (jpeg::read-marker stream) jpeg::+M_SOI+)
        (error "Unrecognized JPEG format"))
      (let* ((marker (jpeg::interpret-markers image 0 stream)))
        (cond ((= jpeg::+M_SOF0+ marker)
               (jpeg::decode-frame image stream nil)
               (jpeg::descriptor-buffer image))
              (t (error "Unsupported JPEG format: ~A" marker)))))))
