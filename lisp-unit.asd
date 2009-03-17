;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Copyright (c) 2009 Thomas M. Hermann
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining 
;;; a copy of this software and associated documentation files (the "Software"), 
;;; to deal in the Software without restriction, including without limitation 
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;;; and/or sell copies of the Software, and to permit persons to whom the 
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included 
;;; in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;

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
