;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;;
;;;; Additional LISP-UNIT tests and assertions
;;;;
;;;; Copyright (c) 2009, Thomas M. Hermann
;;;; All rights reserved.
;;;;
;;;; Redistribution and  use  in  source  and  binary  forms, with or without
;;;; modification, are permitted  provided  that the following conditions are
;;;; met:
;;;;
;;;;   o  Redistributions of  source  code  must  retain  the above copyright
;;;;      notice, this list of conditions and the following disclaimer.
;;;;   o  Redistributions in binary  form  must reproduce the above copyright
;;;;      notice, this list of  conditions  and  the  following disclaimer in
;;;;      the  documentation  and/or   other   materials  provided  with  the
;;;;      distribution.
;;;;   o  The names of the contributors may not be used to endorse or promote
;;;;      products derived from this software without  specific prior written
;;;;      permission.
;;;;
;;;; THIS SOFTWARE IS  PROVIDED  BY  THE  COPYRIGHT  HOLDERS AND CONTRIBUTORS
;;;; "AS IS"  AND  ANY  EXPRESS  OR  IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS FOR A
;;;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;;;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;;; EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT LIMITED TO,
;;;; PROCUREMENT OF  SUBSTITUTE  GOODS  OR  SERVICES;  LOSS  OF USE, DATA, OR
;;;; PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER  CAUSED AND ON ANY THEORY OF
;;;; LIABILITY, WHETHER  IN  CONTRACT,  STRICT  LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR  OTHERWISE)  ARISING  IN  ANY  WAY  OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;
;;;; References
;;;; [NumLinAlg] James W. Demmel "Applied Numerical Linear Algebra",
;;;;             Society for Industrial and Applied Mathematics, 1997
;;;;             ISBN: 0-89871-389-7

(common-lisp:in-package :lisp-unit)

(defparameter *epsilon* nil
  "Set the error epsilon if the defaults are not acceptable.")

(defparameter *significant-figures* 4
  "Default to 4 significant figures.")

;;; (ROUNDOFF-ERROR x y) => number
(defun roundoff-error (exact approximate)
  "Return the error delta between the exact and approximate floating
point value."
  ;; [NumLinAlg] : Equation 1.1, pg. 12
  (abs (if (or (zerop exact) (zerop approximate))
	   (+ exact approximate)
	   (- (/ approximate exact) 1.0))))

;;; (FLOAT-EQUAL float1 float2 &optional epsilon) => true or false
(defun float-equal (float1 float2 &optional (epsilon *epsilon*))
  "Return true if the absolute difference between float1 and float2 is
less than some epsilon."
  (when (and (floatp float1) (floatp float2))
    (cond
      ((and (zerop float1) (zerop float2)))
      (epsilon
       (> epsilon (roundoff-error float1 float2)))
      ((and (typep float1 'double-float) (typep float2 'double-float))
       (> (* 2.0 double-float-epsilon) (roundoff-error float1 float2)))
      ((or (typep float1 'single-float) (typep float2 'single-float))
       (> (* 2.0 single-float-epsilon) (roundoff-error float1 float2)))
      (t nil))))

(defmacro assert-float-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'float-equal))

;;; (COMPLEX-EQUAL complex1 complex2 &optional epsilon) => true or false
(defun complex-equal (complex1 complex2 &optional (epsilon *epsilon*))
  "Return true if the absolute difference between Re(complex1),
Re(complex2) and the absolute difference between Im(complex1),
Im(complex2) is less than epsilon."
  (and
   (typep complex1 '(complex float))
   (typep complex2 '(complex float))
   (float-equal (realpart complex1) (realpart complex2) epsilon)
   (float-equal (imagpart complex1) (imagpart complex2) epsilon)))

(defmacro assert-complex-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'complex-equal))

;;; (NUMBER-EQUAL number1 number2) => true or false
(defun number-equal (number1 number2 &optional (epsilon *epsilon*))
  "Return true if the numbers are equal using the appropriate
comparison."
  (cond
    ((and (floatp number1) (floatp number2))
     (float-equal number1 number2 epsilon))
    ((and (typep number1 '(complex float)) (typep number2 '(complex float)))
     (complex-equal number1 number2 epsilon))
    ((and (numberp number1) (numberp number2))
     (= number1 number2))
    (t (error "~A and ~A are not numbers." number1 number2))))

(defmacro assert-number-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'number-equal))

;;; (NORMALIZE-FLOAT significand &optional exponent) => significand,exponent
(defun normalize-float (significand &optional (exponent 0))
  "Return the normalized floating point number and exponent."
  (cond
    ((zerop significand)
     (values significand 0))
    ((>= (abs significand) 10)
     (normalize-float (/ significand 10.0) (1+ exponent)))
    ((< (abs significand) 1)
     (normalize-float (* significand 10.0) (1- exponent)))
    (t (values significand exponent))))

;;; (SIGFIG-EQUAL float1 float2 significant-figures) => true or false
(defun sigfig-equal (float1 float2 &optional (significant-figures *significant-figures*))
  "Return true if the floating point numbers have equal significant
figures."
  ;; Convert 5 to precision of FLOAT1 and 10 to precision of FLOAT2.
  ;; Then, rely on Rule of Float and Rational Contagion, CLHS 12.1.4.1,
  ;; to obtain a DELTA of the proper precision.
  (let ((delta (* (float 5 float1) (expt (float 10 float2) (- significant-figures)))))
    (if (or (zerop float1) (zerop float2))
	(< (abs (+ float1 float2)) delta)
	(multiple-value-bind (sig1 exp1) (normalize-float float1)
	  (multiple-value-bind (sig2 exp2) (normalize-float float2)
	    (and (= exp1 exp2)
		 (< (abs (- sig1 sig2)) delta)))))))

(defmacro assert-sigfig-equal (significant-figures expected form &rest extras)
  (expand-assert :equal form form expected extras
		 :test (lambda (f1 f2) (sigfig-equal f1 f2 significant-figures))))

;;; (ARRAY-EQUAL array1 array2) => true or false
(defun array-equal (array1 array2 &key (test #'number-equal))
  "Return true if the elements of the array are equal."
  (when (equal (array-dimensions array1) (array-dimensions array2))
    (every test
     (make-array (reduce #'* (array-dimensions array1)) :displaced-to array1)
     (make-array (reduce #'* (array-dimensions array2)) :displaced-to array2))))

(defmacro assert-array-equal (element-test expected form &rest extras)
  (expand-assert :equal form form expected extras
		 :test `(lambda (a1 a2) (array-equal a1 a2 :test ,element-test))))

;;; (NUMERICAL-EQUAL result1 result2) => true or false
;;;
;;; This is a universal wrapper created by Liam Healy. It is
;;; implemented to support testing in GSLL. The interface is expanded,
;;; but backwards compatible with previous versions.
;;;
(defun numerical-equal (result1 result2 &key (test #'number-equal))
  (cond
    ((and (numberp result1) (numberp result2))
     (funcall test result1 result2))
    ((and (typep result1 'sequence) (typep result2 'sequence))
     (when (= (length result1) (length result2))
       (every (lambda (r1 r2) (numerical-equal r1 r2 :test test))
	      result1 result2)))
    ((and (arrayp result1) (arrayp result2))
     (array-equal result1 result2 :test test))
    (t (error "~A and/or ~A are not valid arguments." result1 result2))))

(defmacro assert-numerical-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'numerical-equal))
