;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;;
;;;; lisp-unit
;;;;

(common-lisp:defpackage #:lisp-unit-system
  (:use #:common-lisp #:asdf))

(common-lisp:in-package #:lisp-unit-system)

(defsystem :lisp-unit
  :description "Common Lisp library that supports unit testing."
  :version "Draft"
  :author "Christopher K. Riesbeck <c-riesbeck@northwestern.edu>"
  :license "MIT"
  :components ((:file "defpackage")
	       (:file "lisp-unit"
		      :depends-on ("defpackage"))
	       (:file "floating-point"
		      :depends-on ("defpackage"
				   "lisp-unit"))))
