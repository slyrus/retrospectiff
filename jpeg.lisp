
(in-package :retrospectiff.compression)

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
