;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Floating tests and assertions for LISP-UNIT
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
;;; References
;;; [NumLinAlg] James W. Demmel "Applied Numerical Linear Algebra",
;;;             Society for Industrial and Applied Mathematics, 1997
;;;             ISBN: 0-89871-389-7

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
(defun %float-equal (float1 float2 epsilon)
  "Internal version that does not verify arguments as floats."
  (cond
    ((and (zerop float1) (zerop float2)))
    (epsilon
     (< (roundoff-error float1 float2) epsilon))
    ((or (typep float1 'short-float) (typep float2 'short-float))
     (< (roundoff-error float1 float2) (* 2.0 short-float-epsilon)))
    ((or (typep float1 'single-float) (typep float2 'single-float))
     (< (roundoff-error float1 float2) (* 2.0 single-float-epsilon)))
    ((or (typep float1 'double-float) (typep float2 'double-float))
     (< (roundoff-error float1 float2) (* 2.0 double-float-epsilon)))
    (t (< (roundoff-error float1 float2) (* 2.0 long-float-epsilon)))))

(defun float-equal (float1 float2 &optional (epsilon *epsilon*))
  "Return true if the absolute difference between float1 and float2 is
less than some epsilon."
  (when (and (floatp float1) (floatp float2))
    (%float-equal float1 float2 epsilon)))

(defmacro assert-float-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'float-equal))

;;; (COMPLEX-EQUAL complex1 complex2 &optional epsilon) => true or false
(defun %complex-equal (complex1 complex2 epsilon)
  "Internal version that does not verify arguments as (complex float)."
  (and
   (%float-equal (realpart complex1) (realpart complex2) epsilon)
   (%float-equal (imagpart complex1) (imagpart complex2) epsilon)))

(defun complex-equal (complex1 complex2 &optional (epsilon *epsilon*))
  "Return true if the absolute difference between Re(complex1),
Re(complex2) and the absolute difference between Im(complex1),
Im(complex2) is less than epsilon."
  (when (and (typep complex1 '(complex float))
	     (typep complex2 '(complex float)))
    (%complex-equal complex1 complex2 epsilon)))

(defmacro assert-complex-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'complex-equal))

;;; (NUMBER-EQUAL number1 number2) => true or false
(defun number-equal (number1 number2 &optional (epsilon *epsilon*))
  "Return true if the numbers are equal using the appropriate
comparison."
  (cond
    ((and (floatp number1) (floatp number2))
     (%float-equal number1 number2 epsilon))
    ((and (typep number1 '(complex float)) (typep number2 '(complex float)))
     (%complex-equal number1 number2 epsilon))
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
     (make-array (array-total-size array1)
		 :element-type (array-element-type array1)
		 :displaced-to array1)
     (make-array (array-total-size array2)
		 :element-type (array-element-type array2)
		 :displaced-to array2))))

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
    (t (error "~A and ~A are not valid arguments." result1 result2))))

(defmacro assert-numerical-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'numerical-equal))

;;; Diagnostic functions
;;; Failing a unit test is only half the problem.

;;; Relative floating point error.
(defun float-error (float1 float2)
  "Return the roundoff error of the floating point numbers."
  (if (and (floatp float1) (floatp float2))
      (roundoff-error float1 float2)
      (error "~A and ~A are not float." float1 float2)))

;;; Floating point error as a multiple of epsilon.
(defun %float-error-epsilon (float1 float2 epsilon)
  "Return the roundoff error divided by epsilon."
  (cond
    (epsilon
     (/ (roundoff-error float1 float2) epsilon))
    ((or (typep float1 'short-float) (typep float2 'short-float))
     (/ (roundoff-error float1 float2) short-float-epsilon))
    ((or (typep float1 'single-float) (typep float2 'single-float))
     (/ (roundoff-error float1 float2) single-float-epsilon))
    ((or (typep float1 'double-float) (typep float2 'double-float))
     (/ (roundoff-error float1 float2) double-float-epsilon))
    (t (/ (roundoff-error float1 float2) long-float-epsilon))))

(defun float-error-epsilon (float1 float2 &optional (epsilon *epsilon*))
  "Return the roundoff error divided by epsilon."
  (if (and (floatp float1) (floatp float2))
      (%float-error-epsilon float1 float2 epsilon)
      (error "~A and ~A are not float." float1 float2)))

;;; Relative complex floating point error.
(defun %complex-error (complex1 complex2)
  "Internal version with no type checking."
  (complex
   (roundoff-error (realpart complex1) (realpart complex2))
   (roundoff-error (imagpart complex1) (imagpart complex2))))

(defun complex-error (complex1 complex2)
  "Return the roundoff error of each component of the complex numbers."
  (if (and (typep complex1 '(complex float))
	   (typep complex2 '(complex float)))
      (%complex-error complex1 complex2)
      (error "~A and ~A are not complex float." complex1 complex2)))

;;; Complex floating point error as a multiple of epsilon.
(defun %complex-error-epsilon (complex1 complex2 epsilon)
  "Return the roundoff error of each component divided by epsilon."
  (complex
   (%float-error-epsilon (realpart complex1) (realpart complex2) epsilon)
   (%float-error-epsilon (imagpart complex1) (imagpart complex2) epsilon)))

(defun complex-error-epsilon (complex1 complex2 &optional (epsilon *epsilon*))
  "Return the roundoff error of each component divided by epsilon."
  (if (and (typep complex1 '(complex float))
	   (typep complex2 '(complex float)))
      (%complex-error-epsilon complex1 complex2 epsilon)
      (error "~A and ~A are not complex float." complex1 complex2)))

;;; Numeric roundoff error
(defun number-error (number1 number2)
  "Return the roundoff error of the number."
  (cond
    ((and (floatp number1) (floatp number2))
     (roundoff-error number1 number2))
    ((and (typep number1 '(complex float)) (typep number2 '(complex float)))
     (%complex-error number1 number2))
    ((and (numberp number1) (numberp number2))
     (abs (- (/ number1 number2) 1)))
    (t (error "~A and ~A are not numbers." number1 number2))))

;;; Numeric error as a multiple of epsilon.
(defun number-error-epsilon (number1 number2 &optional (epsilon *epsilon*))
  "Return the roundoff error divided by epsilon."
  (cond
    ((and (floatp number1) (floatp number2))
     (%float-error-epsilon number1 number2 epsilon))
    ((and (typep number1 '(complex float)) (typep number2 '(complex float)))
     (%complex-error-epsilon number1 number2 epsilon))
    (t (error "~A and ~A are not float or complex float numbers." number1 number2))))

;;; Sequence errors and the indices.
(defun %sequence-error (sequence1 sequence2 test error-function)
  "Return a sequence of the indice and error between the sequences."
  (let ((errseq '()))
    (dotimes (index (length sequence1) errseq)
      (let ((n1 (elt sequence1 index))
	    (n2 (elt sequence2 index)))
	(unless (funcall test n1 n2)
	  (push (list index n1 n2 (funcall error-function n1 n2))
		errseq))))))

(defun sequence-error (sequence1 sequence2 &key
		       (test #'number-equal)
		       (error-function #'number-error))
  "Return a sequence of the indice and error between the sequence elements."
  (cond
    ((not (typep sequence1 'sequence))
     (error "SEQUENCE1 is not a valid sequence."))
    ((not (typep sequence2 'sequence))
     (error "SEQUENCE2 is not a valid sequence."))
    ((not (= (length sequence1) (length sequence2)))
     (error "Lengths not equal. SEQUENCE1(~D) /= SEQUENCE2(~D)."
	    (length sequence1) (length sequence2)))
    (t (%sequence-error sequence1 sequence2 test error-function))))

;;; Array errors and the indices.
(defun %array-indices (row-major-index dimensions)
  "Recursively calculate the indices from the row major index."
  (let ((remaining (rest dimensions)))
    (if remaining
	(multiple-value-bind (index remainder)
	    (floor row-major-index (reduce #'* remaining))
	  (cons index (%array-indices remainder remaining)))
	(cons row-major-index nil))))

(defun %array-error (array1 array2 test errfun)
  "Return a list of the indices, values and error of the elements that
are not equal."
  (let ((dimensions (array-dimensions array1))
	(errseq '()))
    (dotimes (index (array-total-size array1) errseq)
      (let* ((indices (%array-indices index dimensions))
	     (n1 (apply #'aref array1 indices))
	     (n2 (apply #'aref array2 indices)))
	(unless (funcall test n1 n2)
	  (push (list indices n1 n2 (funcall errfun n1 n2))
		errseq))))))

(defun array-error (array1 array2 &key
		    (test #'number-equal)
		    (error-function #'number-error))
  "Return a list of the indices and error between the array elements."
  (cond
    ((not (arrayp array1))
     (error "ARRAY1 is not an array."))
    ((not (arrayp array2))
     (error "ARRAY2 is not an array."))
    ((not (equal (array-dimensions array1) (array-dimensions array2)))
     (error "Arrays are not equal dimensions."))
    (t (%array-error array1 array2 test error-function))))
