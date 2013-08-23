;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(cl:defpackage :retrospectiff-test
  (:use #:cl #:retrospectiff #:hu.dwim.stefil)
  (:import-from #:flexi-streams
		#:with-input-from-sequence
		#:with-output-to-sequence))
