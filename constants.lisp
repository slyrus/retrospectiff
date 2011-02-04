
(in-package :retrospectiff)

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

(defconstant +no-compression+ 1)
(defconstant +lzw-compression+ 5)
(defconstant +packbits-compression+ #x8005)


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
(defconstant +field-type-double+ 12)

(defconstant +exif-tag+ 34665)
(defconstant +icc-profile-tag+ 34675)
