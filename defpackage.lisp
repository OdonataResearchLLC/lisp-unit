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

(common-lisp:defpackage #:lisp-unit
  (:use #:common-lisp)
  (:export #:define-test #:run-all-tests #:run-tests
           #:assert-eq #:assert-eql #:assert-equal #:assert-equalp
           #:assert-error #:assert-expands #:assert-false 
           #:assert-equality #:assert-prints #:assert-true
           #:get-test-code #:get-tests
           #:remove-all-tests #:remove-tests
           #:logically-equal #:set-equal
           #:use-debugger
           #:with-test-listener
	   ;; Floating point predicates and assertions
	   #:*epsilon* #:*significant-figures*
	   #:float-equal #:assert-float-equal
	   #:complex-equal #:assert-complex-equal
	   #:number-equal #:assert-number-equal
	   #:sigfig-equal #:assert-sigfig-equal
	   #:array-equal #:assert-array-equal
	   #:numerical-equal #:assert-numerical-equal
	   ;; Floating point diagnostic functions
	   #:float-error #:float-error-epsilon
	   #:complex-error #:complex-error-epsilon
	   #:number-error #:number-error-epsilon
	   #:sequence-error #:array-error))

(pushnew :lisp-unit common-lisp:*features*)
