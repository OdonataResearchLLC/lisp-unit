;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
Copyright (c) 2004-2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :lisp-unit
  (:use :common-lisp)
  (:export :define-test :run-all-tests :run-tests
           :assert-eq :assert-eql :assert-equal :assert-equalp
           :assert-error :assert-expands :assert-false 
           :assert-equality :assert-prints :assert-true
           :get-test-code :get-tests
           :remove-all-tests :remove-tests
           :logically-equal :set-equal
           :use-debugger
           :with-test-listener
           ;; Rational predicates and assertions
           :rational-equal :assert-rational-equal
	   ;; Floating point parameters
	   :*measure* :*epsilon* :*significant-figures*
           ;; Floating point functions
           :default-epsilon :relative-error
           :sumsq :sump :norm
           :relative-error-norm
	   ;; Floating point predicates and assertions
           :float-equal :assert-float-equal
	   :sigfig-equal :assert-sigfig-equal
           :norm-equal :assert-norm-equal
	   :number-equal :assert-number-equal
	   :numerical-equal :assert-numerical-equal
	   ;; Floating point diagnostic functions
	   :sequence-error :array-error
	   ;; Floating point data functions
	   :make-2d-list
	   :complex-random
	   :make-random-list
	   :make-random-2d-list
	   :make-random-2d-array))

(pushnew :lisp-unit common-lisp:*features*)
