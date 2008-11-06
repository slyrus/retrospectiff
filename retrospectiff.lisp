;;; file: retrospectiff.lisp
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

  (defconstant +photometric-interpretation-white-is-zero+ 0)
  (defconstant +photometric-interpretation-black-is-zero+ 1)
  (defconstant +photometric-interpretation-rgb+ 2)

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
  (defconstant +field-type-slong+ 9)
  (defconstant +field-type-srational+ 10)
  (defconstant +field-type-float+ 11)
  (defconstant +field-type-double+ 12))

(defun field-length (tag)
  (case tag
    ((#.+field-type-byte+
      #.+field-type-ascii+
      #.+field-type-sbyte+)
     1)
    ((#.+field-type-short+
      #.+field-type-sshort+)
     2)
    ((#.+field-type-long+
      #.+field-type-slong+
      #.+field-type-float+)
     4)
    ((#.+field-type-rational+
      #.+field-type-srational+
      #.+field-type-double+)
     8)))

(defparameter *byte-order* nil)

(defun vectorize (sequence)
  (if (vectorp sequence)
      sequence
      (map 'vector #'identity sequence)))

(defun listize (sequence)
  (if (listp sequence)
      sequence
      (map 'list #'identity sequence)))

(defun sequencep (x)
  (typep x 'sequence))

(defun read-bytes (stream count)
  (let ((buf (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence buf stream)
    buf))

(defun read-signed-bytes (stream count)
  (let ((buf (make-array count :element-type '(signed-byte 8))))
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

(defun write-int-16-sequence (stream values)
  (if (sequencep values)
      (write-int-16-array stream (vectorize values))
      (write-int-16 stream values)))

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

(defun write-int-32-sequence (stream values)
  (if (sequencep values)
      (write-int-32-array stream (vectorize values))
      (write-int-32 stream values)))

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
      (vectorize
       (loop for i below 4
          for byte = (read-byte stream)
          when (< i count)
          collect byte))
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
      (vectorize
       (loop for i below 2
          for short = (read-int-16 stream)
          when (< i count)
          collect short))
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

(defun get-ifd-entry-values (ifd-entry)
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
      (get-ifd-entry-values field))))

(defun get-ifd-value (ifd key)
  (let ((values (get-ifd-values ifd key)))
    (typecase values
      (list (car values))
      (array (elt values 0)))))

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
    (#.+packbits-compression+
     (let ((packed-bits (read-bytes stream strip-byte-count)))
       (let ((decoded (packbits-decode packed-bits))
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
                           (error "Not yet!"))))))))))))))

(defclass tiff-image ()
  ((length :accessor tiff-image-length :initarg :length)
   (width :accessor tiff-image-width :initarg :width)
   (bits-per-sample :accessor tiff-image-bits-per-sample :initarg :bits-per-sample)
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
                     :bits-per-sample bits-per-sample
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

;; we should return the number of strips (and possibly the length of
;; each strip (uncompressed), but not yet)..
(defun compute-rows-per-strip (image-length
                               bytes-per-row
                               &key (strip-size #x40000))
  (let ((strip-rows (truncate strip-size bytes-per-row)))
    (min image-length strip-rows)))

(defclass ifd-entry ()
  ((tag :accessor ifd-entry-tag :initarg :tag)
   (field-type :accessor ifd-entry-field-type :initarg :field-type)
   (count :accessor ifd-entry-count :initarg :count)
   (values :accessor ifd-entry-values :initarg :values)
   (extra-data :accessor ifd-entry-extra-data :initarg :extra-data)))

;;; To write a TIFF file to a stream we need to make some
;;; decisions. If we can randomly seek into the file, then we can
;;; start writing as we go, seek to the end, retun back to where we
;;; were before and write the address of the data we just stored. This
;;; isn't such a good idea if we're trying to write to a stream where
;;; we can't read (or set) the file-position. Let's see if we can
;;; figure out how to write the file without using file-position.
;;;
;;; TIFF files look like this:
;;; 
;;; 1. 2 bytes for the byte-order flag (#x4949 for big-endian, #x4d4d
;;; for little-endian)
;;;
;;; 2. 2 bytes for the magic 16-bit integer (42) (in the appropriate
;;; endianness).
;;;
;;; 3. 4 bytes for the offset of the first directory entry.
;;; 
;;; Now comes the tricky part. We can either write the image file
;;; directory here, pointing to the actual image data later, or we can
;;; write the image data, squirrel away the position and then put the
;;; image file directory after the image. Let's try writing the
;;; directory first, computing the size of the directory, and manually
;;; calculating the eventual location of the data and storing that in
;;; the image file directory.
;;; 
;;; This means that we can write the 4 bytes for the first directory
;;; entry as #x00000008.
;;;
;;; For the moment let's assume that there will only be one Image File
;;; Directory (IFD) per file.
;;; 
;;; 4. Write the IFD entries.
;;; 
;;; In order to this, we will need to know how many IFD entries there
;;; will be so that we know where to store the data pointed to by the
;;; IFD entries. Therefore we need to add all of the IFD entries to a
;;; list prior to actually writing any of them.
;;;
;;; The IFD entries consist of a 12-byte IFD entry and its associated
;;; data, if any. We will refer to these as the IFD Entry (IFDE) and
;;; the Out-of-line IFDE Values (OIV). If, and only if, I think, an
;;; IFD Entry has more than 4 bytes of data associated with it, the
;;; data are stored in in an OIV, otherwise the data are stored
;;; directly in the IFDE.

;;; All of the IFD entries will appear consecutively in the IFD, each
;;; taking up 12 bytes each. To compute the size of the size of the
;;; IFD and it's associated values, we add 2 + (* 12 IFDE-count) +
;;; [the sum of the length of the OIVs].
;;;
;;; In addition to the IFDE's and their associated OIV's, there will
;;; also be data in the file that isn't in the IFD per se (that is the
;;; IFDE + the OIV), but rather is pointed to by the IFDE values
;;; (either inline or out-of-line). We'll call this the data, for lack
;;; of a better term.

;;; 4a. write the number of IFD entries (2 bytes)
;;; 

(defun write-tiff-stream (stream image &key (byte-order
                                             (or *byte-order* :big-endian)))
  (with-accessors
        ((image-width tiff-image-width)
         (image-length tiff-image-length)
         (samples-per-pixel tiff-image-samples-per-pixel)
         (image-data tiff-image-data))
      image
    (let* ((*byte-order* byte-order)
           (stream-offset 0)
           ifd-entries
           out-of-line-ifd-entries
           oiv-pointer
           (bytes-per-row (* image-width samples-per-pixel))
           (rows-per-strip
            (compute-rows-per-strip image-length bytes-per-row)))
      (destructuring-bind (strip-offsets strip-byte-counts)
          (apply #'mapcar #'list
                 (loop for i below image-length by rows-per-strip
                    for byte-offset from i by (* rows-per-strip
                                                 bytes-per-row) 
                    collect (list byte-offset 
                                  (* bytes-per-row
                                     (- (min (+ i rows-per-strip)
                                             image-length) i))))) 
        (flet ((add-ifd-entry (tag field-type values)
                 (push (make-instance 'ifd-entry
                                      :tag tag
                                      :field-type field-type
                                      :values values) ifd-entries))
               (write-ifd-entry (ifd-entry)
                 (with-accessors ((tag ifd-entry-tag)
                                  (field-type ifd-entry-field-type)
                                  (values ifd-entry-values))
                     ifd-entry
                   (write-int-16 stream tag)
                   (write-int-16 stream field-type)
                   (let* ((field-count (if (sequencep values)
                                           (length values)
                                           1))
                          (field-size (* (field-length field-type)
                                         field-count)))
                     (write-int-32 stream field-count)

                     ;; if the tag is strip-offsets, we need to fix up
                     ;; the location of offsets to point to where the
                     ;; will actually be, which we only know once we
                     ;; start writing out the data.
                       
                     (if (<= field-size 4)
                         (progn
                           (cond
                             ((= field-type +field-type-short+)
                              (write-int-16-sequence stream values))
                             ((= field-type +field-type-ascii+)
                              (error "Can't write inline (<= 4 byte) asciis yet!"))
                             ((= field-type +field-type-long+)
                              (write-int-32-sequence stream values)))
                           (loop for i from field-size below 4
                              do (write-byte 0 stream)))
                         (progn
                           (push ifd-entry out-of-line-ifd-entries)
                           (write-int-32 stream oiv-pointer)
                           (incf oiv-pointer field-size)
                           (when (oddp oiv-pointer)
                             (incf oiv-pointer)))))))
               (write-out-of-line-data (ifd-entry &key data-relative)
                 (with-accessors ((tag ifd-entry-tag)
                                  (field-type ifd-entry-field-type)
                                  (values ifd-entry-values))
                     ifd-entry
                   (when data-relative
                     (setf values (map 'vector
                                       (lambda (x) (+ x oiv-pointer))
                                       values)))
                   (cond
                     ((= field-type +field-type-short+)
                      (write-int-16-sequence stream values))
                     ((= field-type +field-type-ascii+)
                      (error "Can't write inline (<= 4 byte) asciis yet!"))
                     ((= field-type +field-type-long+)
                      (write-int-32-sequence stream values))
                     (t (error "Not yet!")))
                   (incf stream-offset (* (field-length field-type)
                                          (length values)))))
               (write-data (vector)
                 (write-sequence vector stream)
                 (incf stream-offset (length vector))))
          
          ;; 1. Write the byte-order bytes (either #x4949 for
          ;; big-endian, or #4d4d for little-endian).
          (ecase byte-order
            (:little-endian (write-int-16 stream #x4949))
            (:big-endian (write-int-16 stream #x4d4d)))
          (incf stream-offset 2)
          
          ;; 2. Write the magic 16-bit integer 42.
          (write-int-16 stream 42)
          (incf stream-offset 2)
          
          ;; 3. Write the offset of the first directory entry.
          (write-int-32 stream 8)
          (incf stream-offset 4)

          ;; 4. Write the first (and only, for the moment) IFD
          ;; (Image File Directory).
          ;;
          ;; 4a. compute and write the number of directory entries.
          (add-ifd-entry  +image-width-tag+ +field-type-long+ image-width)
          (add-ifd-entry +image-length-tag+ +field-type-long+ image-length)
            
          ;; now we need RowsPerStrip
          ;; FIXME! assume 8 bits per sample for the moment!
          (add-ifd-entry +rows-per-strip-tag+ +field-type-long+ rows-per-strip)
            
          ;; FIXME! assume 8 bits per sample for the moment!
          (add-ifd-entry +bits-per-sample-tag+ +field-type-short+
                         (loop for i below samples-per-pixel collect 8))
            
          (add-ifd-entry +samples-per-pixel-tag+ +field-type-short+
                         samples-per-pixel)
            
          (add-ifd-entry +photometric-interpretation-tag+
                         +field-type-short+
                         +photometric-interpretation-rgb+)

          ;; StripByteCounts and StripOffsets
          (add-ifd-entry +strip-byte-counts-tag+ +field-type-long+
                         strip-byte-counts)
          (add-ifd-entry +strip-offsets-tag+ +field-type-long+
                         strip-offsets)
            
          ;; Finally, reverse the ifd-entries list
          (setf ifd-entries
                (sort ifd-entries #'< :key #'ifd-entry-tag))

          (setf oiv-pointer (+ stream-offset 2 (* (length ifd-entries) 12) 4))
            
          (write-int-16 stream (length ifd-entries))
          (incf stream-offset 2)

          ;; 5. Write the directory entries
          (loop for entry in ifd-entries do (write-ifd-entry entry))
            
          (incf stream-offset (* (length ifd-entries) 12))

          (setf out-of-line-ifd-entries
                (nreverse out-of-line-ifd-entries))
          
          ;; 6. Write 0 to indicate that there are no more IFDs
          (write-int-32 stream 0)
          (incf stream-offset 4)
            
          ;; 7. Write the data for each out-of-line directory entry
          (loop for ifd-entry in out-of-line-ifd-entries
             do (write-out-of-line-data ifd-entry
                                        :data-relative 
                                        (= (ifd-entry-tag ifd-entry)
                                           +strip-offsets-tag+)))
          ;; 8. Now write out the strip data
          (loop for start in strip-offsets
             for count in strip-byte-counts
             do (write-data (subseq image-data start
                                    (+ start count)))))))))

(defmacro write-tiff-file (pathname image &rest args)
  (let ((stream (gensym "write-tiff-file")))
    `(with-open-file (,stream ,pathname
                             :direction :output
                             :element-type :default
                             ,@args)
       (write-tiff-stream ,stream ,image)
       ,pathname)))

(defparameter *image-info-attributes*
  `((,+image-width-tag+ "Image Width")
    (,+image-length-tag+ "Image Length")
    (,+x-resolution-tag+ "X Resolution")
    (,+y-resolution-tag+ "Y Resolution")
    (,+samples-per-pixel-tag+ "Samples per Pixel")
    (,+bits-per-sample-tag+ "Bits per Sample")
    (,+rows-per-strip-tag+ "Rows per Strip")
    (,+strip-offsets-tag+ "Strip Offsets")
    (,+compression-tag+
     "Compression" 
     ,(lambda (description values)
              (cons description 
                    (cdr (assoc
                          (elt values 0)
                          `((,+packbits-compression+ . "Packbits")
                            (,+lzw-compression+ . "LZW")))))))))

(defun image-info (stream ifd)
  (declare (optimize (debug 2)))
  (let ((rows-per-strip (get-ifd-value ifd +rows-per-strip-tag+))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+))
        (compression (get-ifd-value ifd +compression-tag+))
        (planar-configuration (get-ifd-value ifd +planar-configuration-tag+))
        (predictor (get-ifd-value ifd +predictor-tag+)))
    (append
     (list (cons "Byte Order: " *byte-order*))
     (loop for attr in *image-info-attributes*
        collect
        (destructuring-bind (tag description &optional fn)
            attr
          (if fn
              (funcall fn description (get-ifd-values ifd tag))
              (cons description (get-ifd-values ifd tag))))))))

(defun read-tiff-stream-info (stream)
  (let ((*byte-order*))
    (let ((byte-order (read-int-16 stream)))
      (ecase byte-order
        (#x4949 (setf *byte-order* :little-endian))
        (#x4d4d (setf *byte-order* :big-endian))))
    (let ((tiff-magic (read-int-16 stream)))
      (unless (= tiff-magic 42)
        (error "Error reading TIFF file. ~S is not 42." tiff-magic)))
    (let ((ifds (read-ifds stream)))
      (image-info stream (car ifds)))))

(defun read-tiff-file-info (pathname)
  (with-open-file (stream pathname :direction :input :element-type :default)
    (read-tiff-stream-info stream)))

