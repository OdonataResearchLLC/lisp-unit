;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;; References
;;;; [NumLinAlg] James W. Demmel "Applied Numerical Linear Algebra",
;;;;             Society for Industrial and Applied Mathematics, 1997
;;;;             ISBN: 0-89871-389-7

(common-lisp:in-package :lisp-unit)

;;; (ROUNDOFF-ERROR x y) => number
;;; Return the error delta between the exact and approximate floating
;;; point value.
;;; [NumLinAlg] : Equation 1.1, pg. 12
(defun roundoff-error (exact approximate)
  "Returned the error delta between the exact and approximate floating
point value."
  (abs (if (or (zerop exact) (zerop approximate))
	   (+ exact approximate)
	   (- (/ approximate exact) 1.0))))

;;; (FLOAT-EQUAL float1 float2 &optional epsilon) => true or false
;;; Return true if the absolute difference between float1 and float2
;;; is less than epsilon. If an epsilon is not specified and either
;;; float1 or float2 is single precision, the single-float-epsilon is
;;; used.
(defun float-equal (float1 float2 &optional epsilon)
  "Return true if the absolute difference between float1 and float2 is
less than some epsilon."
  (and
   (floatp float1)
   (floatp float2)
   (cond
     ((and (zerop float1) (zerop float2)))
     (epsilon
      (> epsilon (roundoff-error float1 float2)))
     ((and (typep float1 'double-float) (typep float2 'double-float))
      (> (* 2.0 double-float-epsilon) (roundoff-error float1 float2)))
     ((or (typep float1 'single-float) (typep float2 'single-float))
      (> (* 2.0 single-float-epsilon) (roundoff-error float1 float2)))
     (t nil))))

;;; (COMPLEX-EQUAL complex1 complex2 &optional epsilon) => true or false
;;; Return true if the absolute difference of the real components and
;;; the absolute difference of the imaginary components is less then
;;; epsilon. If an epsilon is not specified and either complex1 or
;;; complex2 is (complex single-float), the single-float-epsilon is
;;; used.
(defun complex-equal (complex1 complex2 &optional epsilon)
  "Return true if the absolute difference between Re(complex1),
Re(complex2) and the absolute difference between Im(complex1),
Im(complex2) is less than epsilon."
  (and
   (typep complex1 '(complex float))
   (typep complex2 '(complex float))
   (float-equal (realpart complex1) (realpart complex2) epsilon)
   (float-equal (imagpart complex1) (imagpart complex2) epsilon)))

;;; (NUMBER-EQUAL number1 number2) => true or false
;;; Return true if the numbers are equal using the appropriate
;;; comparison.
(defun number-equal (number1 number2 &optional epsilon)
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

;;; (ELEMENT-EQUAL array1 array2 indice dimensions) => true or false
;;; A utility function for ARRAY-EQUAL.
(defun element-equal (array1 array2 indices dimensions &key (test #'number-equal))
  "Return true if the index of array1 equals array2."
  (let* ((rank (first dimensions))
	 (remaining (rest dimensions))
	 (update-result
	  (if remaining
	      (lambda (index)
		(element-equal array1 array2
			       (cons index indices) remaining :test test))
	      (lambda (index)
		(funcall test
			 (apply #'aref array1 index (reverse indices))
			 (apply #'aref array2 index (reverse indices)))))))
    (do ((index 0 (1+ index))
	 (result t (funcall update-result index)))
	((or (not result) (>= index rank)) result))))

;;; (ARRAY-EQUAL array1 array2) => true or false
;;; Return true of the elements of the array are equal.
(defun array-equal (array1 array2 &key (test #'number-equal))
  "Return true if the elements of the array are equal."
  (when (equal (array-dimensions array1) (array-dimensions array2))
    (element-equal array1 array2 nil (array-dimensions array1) :test test)))

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

;;; (SIGNIFICANT-FIGURES-EQUAL float1 float2 significant-figures) => true or false
(defun significant-figures-equal (float1 float2 significant-figures)
  "Return true if the floating point numbers have equal significant
figures."
  ;; Convert 5 to precision of FLOAT1 and 10 to precision of
  ;; FLOAT2. Then, rely on Rule of Float and Rational Contagion, CLHS
  ;; 12.1.4.1, to obtain a DELTA of the proper precision.
  (let ((delta (* (float 5 float1) (expt (float 10 float2) (- significant-figures)))))
    (if (or (zerop float1) (zerop float2))
	(< (abs (+ float1 float2)) delta)
	(multiple-value-bind (sig1 exp1) (normalize-float float1)
	  (multiple-value-bind (sig2 exp2) (normalize-float float2)
	    (and (= exp1 exp2)
		 (< (abs (- sig1 sig2)) delta)))))))

(defun 2-sigfig-equal (float1 float2)
  "Return true if the floats are equal to 2 significant figures."
  (significant-figures-equal float1 float2 2))

(defun 3-sigfig-equal (float1 float2)
  "Return true if the floats are equal to 3 significant figures."
  (significant-figures-equal float1 float2 3))

(defun 4-sigfig-equal (float1 float2)
  "Return true if the floats are equal to 4 significant figures."
  (significant-figures-equal float1 float2 4))

(defun 5-sigfig-equal (float1 float2)
  "Return true if the floats are equal to 5 significant figures."
  (significant-figures-equal float1 float2 5))

(defun 6-sigfig-equal (float1 float2)
  "Return true if the floats are equal to 6 significant figures."
  (significant-figures-equal float1 float2 6))
