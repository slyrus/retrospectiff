;;; file: deflate.lisp
;;; author: cyrus harmon
;;;
;;; Copyright (c) 2016 Cyrus Harmon (ch-lisp@bobobeach.com)
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

;; Uses Pierre Mai's DEFLATE package to decode the vector of DEFLATED
;; bytes.
(defun deflate-decode (compressed-vector)
  (let ((input-stream (flexi-streams:make-in-memory-input-stream
                       compressed-vector))
        (output-stream (flexi-streams:make-in-memory-output-stream)))
    ;; There's an extra byte for flags according to the TIFF spec. Not
    ;; sure what we should do with it.
    (let ((cmf (read-byte input-stream))
          (flags (read-byte input-stream)))
      (declare (ignore cmf flags))
      (deflate:inflate-stream input-stream output-stream)
      (flexi-streams:get-output-stream-sequence output-stream))))

