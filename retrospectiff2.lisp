
(cl:defpackage #:retrospectiff2
  (:use #:cl
        #:com.gigamonkeys.binary-data
        #:com.gigamonkeys.binary-data.common-datatypes))

(in-package :retrospectiff2)

(defparameter *byte-order* nil)

;;; Perhaps the next few types should be moved to a
;;; binary-data-extensions file or some such?
(define-binary-type array (type size)
  (:reader (in)
           (let ((arr (make-array size :element-type type)))
             (dotimes (i size)
               (setf (elt arr i)
                     (read-value type in)))
             arr))
  (:writer (out value)
           (dotimes (i (length value))
             (write-value type out (elt value i)))))

(define-binary-type unsigned-integer* (bytes bits-per-byte)
  (:reader (in)
           (case *byte-order*
             (:big-endian
              (loop with value = 0
                 for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return value)))
             (:little-endian
              (loop with value = 0
                 for low-bit to (* bits-per-byte (1- bytes)) by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return value)))))
  (:writer (out value)
           (case *byte-order*
             (:big-endian
              (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))
             (:little-endian
              (loop for low-bit to  (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))))

(define-binary-type u2* () (unsigned-integer* :bytes 2 :bits-per-byte 8))
(define-binary-type u4* () (unsigned-integer* :bytes 4 :bits-per-byte 8))

;;; end binary-data-extensions section
;;;

(define-binary-type tiff-byte-order ()
  (:reader (in)
           (let ((val (read-value 'u2 in)))
             (case val
               (#x4949 (setf *byte-order* :little-endian))
               (#x4D4D (setf *byte-order* :big-endian))
               (t (error "unknown byte order")))))
  (:writer (out value)
           (case (or *byte-order* value)
             (:little-endian
              (write-value 'u2 out #x4949))
             (:big-endian
              (write-value 'u2 out #x4d4d)))))

(define-binary-class ifd-entry ()
  ((tag u2*)
   (field-type u2*)
   (value-count u4*)
   (value-offset u4*)))

(define-binary-type tiff-ifd-offset ()
  (:reader (in)
           (let ((val (read-value 'u4* in)))
             (prog1 val
               (if (plusp val)
                   (file-position in val)))))
  (:writer (out value)
           (write-value 'u4* out value)
           (if (plusp value)
               (file-position out value))))

(define-binary-class ifd ()
  ((entry-count u2*)
   (entries (array :type 'ifd-entry :size entry-count))
   (next-ifd-offset tiff-ifd-offset)))

(define-binary-type ifd-list ()
  (:reader (in)
           (loop for ifd = (read-value 'ifd in)
              collect ifd
              while (plusp (next-ifd-offset ifd))))
  (:writer (out value)
           (loop for ifd in value
              do (write-value 'ifd out ifd))))

(define-binary-class tiff ()
  ((byte-order tiff-byte-order)
   (magic u2*)
   (ifd-offset tiff-ifd-offset)
   (ifd-list ifd-list)))

(defmethod read-value :around ((type (eql 'tiff)) stream &key)
  (let (*byte-order*)
    (call-next-method)))

(defun read-tiff-stream (stream)
  (read-value 'tiff stream))

(defun write-tiff-stream (stream obj &key (byte-order :big-endian))
  (let ((*byte-order* byte-order))
    (write-value 'tiff stream obj)))

