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
	   #:float-equal #:complex-equal #:number-equal
	   #:array-equal
	   #:significant-figures-equal
	   #:3-sigfig-equal #:4-sigfig-equal
	   #:5-sigfig-equal #:6-sigfig-equal
           #:use-debugger
           #:with-test-listener))

(pushnew :lisp-unit common-lisp:*features*)
