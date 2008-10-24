
(cl:defpackage #:retrospectiff
  (:use #:cl)
  (:nicknames #:tiff)
  (:export #:read-tiff-stream
           #:read-tiff-file
           
           #:tiff-image
           #:tiff-image-length
           #:tiff-image-width
           #:tiff-image-samples-per-pixel
           #:tiff-image-data))

