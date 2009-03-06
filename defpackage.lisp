;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

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
	   #:*epsilon* #:*significant-figures* #:*element-test*
	   #:float-equal #:assert-float-equal
	   #:complex-equal #:assert-complex-equal
	   #:number-equal #:assert-number-equal
	   #:sigfig-equal #:assert-sigfig-equal
	   #:array-equal #:assert-array-equal
	   #:numerical-equal #:assert-numerical-equal))

(pushnew :lisp-unit common-lisp:*features*)
