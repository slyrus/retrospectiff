
(in-package :retrospectiff)

(defun add-ifd-entry (ifd entry)
  (push entry (entries ifd))
  (incf (entry-count ifd))
  ifd)

(defun vectorize (data)
  (etypecase data
    (vector data)
    (list (apply #'vector data))
    (nil nil)
    (atom (vector data))))

(defun make-ifd-entry-short (tag data)
  (let ((data (vectorize data)))
    (make-instance 'short-ifd-entry
                   :tag tag
                   :field-type +field-type-short+
                   :data data
                   :value-count (length data))))

(defun make-ifd-entry-long (tag data)
  (let ((data (vectorize data)))
    (make-instance 'long-ifd-entry
                   :tag tag
                   :field-type +field-type-long+
                   :data data
                   :value-count (length data))))

(defun make-ifd-entry-rational (tag data)
  (let ((data (vectorize data)))
    (make-instance 'rational-ifd-entry
                   :tag tag
                   :field-type +field-type-rational+
                   :data (map 'vector
                              (lambda (x)
                                (make-instance 'rational :numerator (car x)
                                               :denominator (cdr x)))
                              data)
                   :value-count (length data))))


(defun ifd-entry-out-of-line-bytes (entry)
  (let ((bytes (entry-bytes entry)))
    (if (> bytes 4) bytes 0)))

