;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
#|

  Floating tests and assertions for LISP-UNIT

  Copyright (c) 2009-2011, Thomas M. Hermann

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

  References
  [NumAlgoC] Gisela Engeln-Mullges and Frank Uhlig "Numerical
             Algorithms with C", Springer, 1996
             ISBN: 3-540-60530-4

|#

(in-package :lisp-unit)

;;; Symbols exported from the floating point extension

(export
 '(*measure* *epsilon* *significant-figures* ; Global variables
   ;; Functions
   default-epsilon relative-error
   sumsq sump norm relative-error-norm
   ;; Predicates and assertions
   float-equal assert-float-equal
   sigfig-equal assert-sigfig-equal
   norm-equal assert-norm-equal
   number-equal assert-number-equal
   numerical-equal assert-numerical-equal))

;;; Floating point extensions

(defvar *measure* 1)

(defvar *epsilon* nil
  "Set the error epsilon if the defaults are not acceptable.")

(defvar *significant-figures* 4
  "Default to 4 significant figures.")

(defgeneric default-epsilon (value)
  (:documentation
   "Return the default epsilon for the value."))

(defgeneric relative-error (exact approximate)
  (:documentation
   "Return the relative-error between the 2 quantities."))

(defgeneric float-equal (data1 data2 &optional epsilon)
  (:documentation
   "Return true if the floating point data is equal."))

(defgeneric sumsq (data)
  (:documentation
   "Return the scaling parameter and the sum of the squares of the ~
    data."))

(defgeneric sump (data p)
  (:documentation
   "Return the scaling parameter and the sum of the powers of p of the ~
    data."))

(defgeneric norm (data &optional measure)
  (:documentation
   "Return the element-wise norm of the data."))

(defgeneric relative-error-norm (exact approximate &optional measure)
  (:documentation
   "Return the relative error norm "))

(defgeneric norm-equal (data1 data2 &optional epsilon measure)
  (:documentation
   "Return true if the norm of the data is equal."))

(defgeneric sigfig-equal (data1 data2 &optional significant-figures)
  (:documentation
   "Return true if the data have equal significant figures."))

(defgeneric numerical-equal (result1 result2 &key test)
  (:documentation
   "Return true if the results are numerically equal according to :TEST."))

;;; (DEFAULT-EPSILON value) => epsilon
(defmethod default-epsilon ((value float))
  "Return a default epsilon value based on the floating point type."
  (typecase value
    (short-float  (* 2S0 short-float-epsilon))
    (single-float (* 2F0 single-float-epsilon))
    (double-float (* 2D0 double-float-epsilon))
    (long-float   (* 2L0 long-float-epsilon))))

(defmethod default-epsilon ((value complex))
  "Return a default epsilon value based on the complex type."
  (typecase value
    ((complex short-float)  (* 2S0 short-float-epsilon))
    ((complex single-float) (* 2F0 single-float-epsilon))
    ((complex double-float) (* 2D0 double-float-epsilon))
    ((complex long-float)   (* 2L0 long-float-epsilon))
    (t 0)))

;;; FIXME : Use the LOOP
(defmethod default-epsilon ((value list))
  "Return the default epsilon based on contents of the list."
  (reduce (lambda (x y) (max x (default-epsilon y)))
          value :initial-value 0))

;;; FIXME : Use the LOOP
(defmethod default-epsilon ((value vector))
  "Return the default epsilon based on the contents of the vector."
  (reduce (lambda (x y) (max x (default-epsilon y)))
          value :initial-value 0))

;;; FIXME : Use the LOOP
(defmethod default-epsilon ((value array))
  "Return the default epsilon based on the contents of the array."
  (reduce (lambda (x y) (max x (default-epsilon y)))
          (make-array (array-total-size value)
                      :element-type (array-element-type value)
                      :displaced-to value)
          :initial-value 0))

#|
  (RELATIVE-ERROR x y) => float
  [NumAlgoC] : Definition 1.3, pg. 2
               modified with Definition 1.1, pg. 1
 
  The definition of relative error in this routine is modified from
  the Definition 1.3 in [NumAlgoC] for cases when either the exact
  or the approximate value equals zero. According to Definition 1.3,
  the relative error is identically equal to 1 in those cases. This
  function returns the absolue error in those cases. This is more
  useful for testing.
|#
(defun %relative-error (exact approximate)
  "Return the relative error of the numbers."
  (abs (if (or (zerop exact) (zerop approximate))
	   (- exact approximate)
	   (/ (- exact approximate) exact))))

(defmethod relative-error ((exact float) (approximate float))
  "Return the error delta between the exact and approximate floating
point value."
  (%relative-error exact approximate))

(defmethod relative-error ((exact float) (approximate complex))
  "Return the relative error between the float and complex number."
  (%relative-error exact approximate))

(defmethod relative-error ((exact complex) (approximate float))
  "Return the relative error between the float and complex number."
  (%relative-error exact approximate))

(defmethod relative-error ((exact complex) (approximate complex))
  "Return the relative error of the complex numbers."
  (if (or (typep exact '(complex float))
          (typep approximate '(complex float)))
      (%relative-error exact approximate)
      (error "Relative error is only applicable to complex values with ~
              floating point parts.")))

;;; (FLOAT-EQUAL data1 data2 epsilon) => true or false
(defun %float-equal (data1 data2 epsilon)
  "Return true if the relative error between the data is less than
epsilon."
  (or
   (and (zerop data1) (zerop data2))
   (< (%relative-error data1 data2) epsilon)))

(defmethod float-equal ((data1 float) (data2 float)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal data1 data2
                (or epsilon (max (default-epsilon data1)
                                 (default-epsilon data2)))))

(defmethod float-equal ((data1 float) (data2 rational)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal data1 (float data2 data1)
                (or epsilon (default-epsilon data1))))

(defmethod float-equal ((data1 rational) (data2 float)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal (float data1 data2) data2
                (or epsilon (default-epsilon data2))))

(defmethod float-equal ((data1 float) (data2 complex)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal data1 data2
                (or epsilon (max (default-epsilon data1)
                                 (default-epsilon data2)))))

(defmethod float-equal ((data1 complex) (data2 float)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal data1 data2
                (or epsilon (max (default-epsilon data1)
                                 (default-epsilon data2)))))

(defmethod float-equal ((data1 complex) (data2 complex)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (< (relative-error data1 data2)
     (or epsilon (max (default-epsilon data1)
                      (default-epsilon data2)))))

(defun %seq-float-equal (seq1 seq2 epsilon)
  "Return true if the element-wise comparison of relative error is
less than epsilon."
  (or
   (and (null seq1) (null seq2))
   (when (= (length seq1) (length seq2))
     (every
      (lambda (d1 d2) (float-equal d1 d2 epsilon)) seq1 seq2))))

(defmethod float-equal ((data1 list) (data2 list)
                        &optional (epsilon *epsilon*))
  "Return true if the lists are equal in length and element-wise
comparison of the relative error is less than epsilon."
  (%seq-float-equal data1 data2 epsilon))

(defmethod float-equal ((data1 list) (data2 vector)
                        &optional (epsilon *epsilon*))
  "Return true if the vector and the list are equal in length and
element-wise comparison of the relative error is less than epsilon."
  (%seq-float-equal data1 data2 epsilon))

(defmethod float-equal ((data1 vector) (data2 list)
                        &optional (epsilon *epsilon*))
  "Return true if the vector and the list are equal in length and
element-wise comparison of the relative error is less than epsilon."
  (%seq-float-equal data1 data2 epsilon))

(defmethod float-equal ((data1 vector) (data2 vector)
                        &optional (epsilon *epsilon*))
  "Return true if the vectors are equal in length and element-wise
comparison of the relative error is less than epsilon."
  (%seq-float-equal data1 data2 epsilon))

(defmethod float-equal ((data1 array) (data2 array)
                        &optional (epsilon *epsilon*))
  "Return true if the arrays are equal in length and element-wise
comparison of the relative error is less than epsilon."
  (when (equal (array-dimensions data1)
               (array-dimensions data2))
    (%seq-float-equal
     (make-array (array-total-size data1)
                 :element-type (array-element-type data1)
                 :displaced-to data1)
     (make-array (array-total-size data2)
                 :element-type (array-element-type data2)
                 :displaced-to data2)
     epsilon)))

(defmacro assert-float-equal (expected form &rest extras)
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'float-equal))

;;; (SUMSQ data) => scale, sumsq
(defmethod sumsq ((data list))
  "Return the scaling parameter and the sum of the squares of the ~
   list."
  (let ((scale 0) (sumsq 1)
        (abs-val nil))
    (dolist (elm data (values scale sumsq))
      (when (< 0 (setf abs-val (abs elm)))
        (if (< scale abs-val)
            (setf sumsq (1+ (* sumsq (expt (/ scale abs-val) 2)))
                  scale abs-val)
            (incf sumsq (expt (/ elm scale) 2)))))))

(defmethod sumsq ((data vector))
  "Return the scaling parameter and the sum of the squares of the ~
   vector."
  (let ((scale 0) (sumsq 1)
        (size (length data))
        (abs-val nil))
    (dotimes (index size (values scale sumsq))
      (when (< 0 (setf abs-val (abs (svref data index))))
        (if (< scale abs-val)
            (setf sumsq (1+ (* sumsq (expt (/ scale abs-val) 2)))
                  scale abs-val)
            (incf sumsq (expt (/ (svref data index) scale) 2)))))))

(defmethod sumsq ((data array))
  "Return the scaling parameter and the sum of the squares of the ~
   array."
  (sumsq (make-array (array-total-size data)
                     :element-type (array-element-type data)
                     :displaced-to data)))

;;; (SUMP data) => scale, sump
(defmethod sump ((data list) (p real))
  "Return the scaling parameter and the sum of the powers of p of the ~
   data."
  (let ((scale 0) (sump 1)
        (abs-val nil))
    (dolist (elm data (values scale sump))
      (when (< 0 (setf abs-val (abs elm)))
        (if (< scale abs-val)
            (setf sump  (1+ (* sump (expt (/ scale abs-val) p)))
                  scale abs-val)
            (incf sump (expt (/ elm scale) p)))))))

(defmethod sump ((data vector) (p real))
  "Return the scaling parameter and the sum of the powers of p of the ~
   vector."
  (let ((scale 0) (sump 1)
        (size (length data))
        (abs-val nil))
    (dotimes (index size (values scale sump))
      (when (< 0 (setf abs-val (abs (svref data index))))
        (if (< scale abs-val)
            (setf sump  (1+ (* sump (expt (/ scale abs-val) p)))
                  scale abs-val)
            (incf sump (expt (/ (svref data index) scale) p)))))))

(defmethod sump ((data array) (p real))
  "Return the scaling parameter and the sum of the powers of p of the ~
   array."
  (sump (make-array (array-total-size data)
                    :element-type (array-element-type data)
                    :displaced-to data)
        p))

;;; (NORM data) => float
(defun %seq-1-norm (data)
  "Return the Taxicab norm of the sequence."
  ;; FIXME : Use the LOOP.
  (reduce (lambda (x y) (+ x (abs y)))
          data :initial-value 0))

(defun %seq-2-norm (data)
  "Return the Euclidean norm of the sequence."
  (multiple-value-bind (scale sumsq)
      (sumsq (map-into (make-array (length data)) #'abs data))
    (* scale (sqrt sumsq))))

(defun %seq-p-norm (data p)
  "Return the p norm of the sequence."
  (multiple-value-bind (scale sump)
      (sump (map-into (make-array (length data)) #'abs data) p)
    (* scale (expt sump (/ p)))))

(defun %seq-inf-norm (data)
  "Return the infinity, or maximum, norm of the sequence."
  ;; FIXME : Use the LOOP.
  (reduce (lambda (x y) (max x (abs y)))
          data :initial-value 0))

(defun %seq-norm (data measure)
  "Return the norm of the sequence according to the measure."
  (cond
    ((equalp measure 1)
     (%seq-1-norm data))
    ((equalp measure 2)
     (%seq-2-norm data))
    ((numberp measure)
     (%seq-p-norm data measure))
    ((equalp measure :infinity)
     (%seq-inf-norm data))
    (t (error "Unrecognized norm, ~A." measure))))

(defmethod norm ((data list) &optional (measure *measure*))
  "Return the norm of the list according to the measure."
  (%seq-norm data measure))

(defmethod norm ((data vector) &optional (measure *measure*))
  "Return the norm of the vector according to the measure."
  (%seq-norm data measure))

(defmethod norm ((data array) &optional (measure *measure*))
  "Return the entrywise norm of the array according to the measure."
  (let ((flat-data (make-array (array-total-size data)
                               :element-type (array-element-type data)
                               :displaced-to data)))
    (cond
      ((and (numberp measure) (< 0 measure))
       (warn "Measure ~D results in an entrywise p-norm." measure)
       (%seq-p-norm flat-data measure))
      ((equalp measure :frobenius)
       (%seq-2-norm flat-data))
      ((equalp measure :max)
       (%seq-inf-norm flat-data))
      (t (error "Unrecognized norm, ~A." measure)))))

;;; (RELATIVE-ERROR-NORM exact approximate measure) => float
(defun %relative-error-norm (exact approximate measure)
  "Return the relative error norm of the sequences."
  (/ (norm (map-into (make-array (length exact))
                     (lambda (x1 x2) (abs (- x1 x2)))
                     exact approximate) measure)
     (norm exact measure)))

(defmethod relative-error-norm ((exact list) (approximate list)
                                &optional (measure *measure*))
  "Return the relative error norm of the lists."
  (if (= (length exact) (length approximate))
      (%relative-error-norm exact approximate measure)
      (error "Lists are not equal in length.")))

(defmethod relative-error-norm ((exact list) (approximate vector)
                                &optional (measure *measure*))
  "Return the relative error norm of the list and the vector."
  (if (= (length exact) (length approximate))
      (%relative-error-norm exact approximate measure)
      (error "The list and vector are not equal in length.")))

(defmethod relative-error-norm ((exact vector) (approximate list)
                                &optional (measure *measure*))
  "Return the relative error norm of the list and the vector."
  (if (= (length exact) (length approximate))
      (%relative-error-norm exact approximate measure)
      (error "The list and vector are not equal in length.")))

(defmethod relative-error-norm ((exact vector) (approximate vector)
                                &optional (measure *measure*))
  "Return the relative error norm of the vectors."
  (if (= (length exact) (length approximate))
      (%relative-error-norm exact approximate measure)
      (error "Vectors are not equal in length.")))

(defmethod relative-error-norm ((exact array) (approximate vector)
                                &optional (measure *measure*))
  "Return the relative error norm of the arrays."
  (if (equal (array-dimensions exact)
             (array-dimensions approximate))
      (%relative-error-norm
       (make-array (array-total-size exact)
                   :element-type (array-element-type exact)
                   :displaced-to exact)
       (make-array (array-total-size approximate)
                   :element-type (array-element-type approximate)
                   :displaced-to approximate)
       measure)
      (error "Arrays are not equal dimensions.")))

;;; (NORM-EQUAL data1 data2 epsilon measure) => boolean
(defun %norm-equal (seq1 seq2 epsilon measure)
  "Return true if the relative error norm is less than epsilon."
  (or
   (and (null seq1) (null seq2))
   (< (%relative-error-norm seq1 seq2 measure) epsilon)))

(defmethod norm-equal ((data1 list) (data2 list) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the lists are equal in length and the relative error
norm is less than epsilon."
  (%norm-equal data1 data2 epsilon measure))

(defmethod norm-equal ((data1 list) (data2 vector) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the vector and the list are equal in length and the
relative error norm is less than epsilon."
  (%norm-equal data1 data2 epsilon measure))

(defmethod norm-equal ((data1 vector) (data2 list) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the vector and the list are equal in length and the
relative error norm is less than epsilon."
  (%norm-equal data1 data2 epsilon measure))

(defmethod norm-equal ((data1 vector) (data2 vector) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the vectors are equal in length and the relative
error norm is less than epsilon."
  (%norm-equal data1 data2 epsilon measure))

(defmethod norm-equal ((data1 array) (data2 array) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the arrays are equal in length and the relative
error norm is less than epsilon."
  (when (equal (array-dimensions data1)
               (array-dimensions data2))
    (%norm-equal
     (make-array (array-total-size data1)
                 :element-type (array-element-type data1)
                 :displaced-to data1)
     (make-array (array-total-size data2)
                 :element-type (array-element-type data2)
                 :displaced-to data2)
     epsilon measure)))

(defmacro assert-norm-equal (expected form &rest extras)
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'norm-equal))

;;; (NORMALIZE-FLOAT significand &optional exponent) => significand,exponent
;;; [NumAlgoC] : Definition 1.7, pg. 4
;;;
;;; To avoid using 0.1, first 1.0 <= significand < 10. On the final
;;; return, scale 0.1 <= significand < 1.
(defun %normalize-float (significand &optional (exponent 0))
  "Return the normalized floating point number and exponent."
  ;;; FIXME : Use the LOOP.
  (cond
    ((zerop significand)
     (values significand 0))
    ((>= (abs significand) 10)
     (%normalize-float (/ significand 10.0) (1+ exponent)))
    ((< (abs significand) 1)
     (%normalize-float (* significand 10.0) (1- exponent)))
    (t (values (/ significand 10.0) (1+ exponent)))))

;;; (SIGFIG-EQUAL float1 float2 significant-figures) => true or false
(defun %sigfig-equal (float1 float2 significant-figures)
  "Return true if the floating point numbers have equal significant
figures."
  (if (or (zerop float1) (zerop float2))
      (< (abs (+ float1 float2)) (* 5D-1 (expt 1D1 (- significant-figures))))
      (multiple-value-bind (sig1 exp1) (%normalize-float float1)
        (multiple-value-bind (sig2 exp2) (%normalize-float float2)
          (= (round (* sig1 (expt 1D1 significant-figures)))
             (round (* sig2 (expt 1D1 (- significant-figures (- exp1 exp2))))))))))

(defmethod sigfig-equal ((data1 float) (data2 float) &optional
                         (significant-figures *significant-figures*))
  "Return true if the floating point numbers have equal significant
figures."
  (%sigfig-equal data1 data2 significant-figures))

(defun %seq-sigfig-equal (seq1 seq2 significant-figures)
  "Return true if the element-wise comparison is equal to the
specified significant figures."
  (or
   (and (null seq1) (null seq2))
   (when (= (length seq1) (length seq2))
     (every
      (lambda (d1 d2) (sigfig-equal d1 d2 significant-figures))
      seq1 seq2))))

(defmethod sigfig-equal ((data1 list) (data2 list) &optional
                         (significant-figures *significant-figures*))
  "Return true if the lists are equal in length and the element-wise
comparison is equal to significant figures."
  (%seq-sigfig-equal data1 data2 significant-figures))

(defmethod sigfig-equal ((data1 vector) (data2 list) &optional
                         (significant-figures *significant-figures*))
  "Return true if the vector and the list are equal in length and the
element-wise comparison is equal to significant figures."
  (%seq-sigfig-equal data1 data2 significant-figures))

(defmethod sigfig-equal ((data1 list) (data2 vector) &optional
                         (significant-figures *significant-figures*))
  "Return true if the list and the vector are equal in length and the
element-wise comparison is equal to significant figures."
  (%seq-sigfig-equal data1 data2 significant-figures))

(defmethod sigfig-equal ((data1 vector) (data2 vector) &optional
                         (significant-figures *significant-figures*))
  "Return true if the vectors are equal in length and the element-wise
comparison is equal to significant figures."
  (%seq-sigfig-equal data1 data2 significant-figures))

(defmethod sigfig-equal ((data1 array) (data2 array) &optional
                         (significant-figures *significant-figures*))
  "Return true if the arrays are equal in length and the element-wise
comparison is equal to significant figures."
  (when (equal (array-dimensions data1)
               (array-dimensions data2))
    (%seq-sigfig-equal
     (make-array (array-total-size data1)
                 :element-type (array-element-type data1)
                 :displaced-to data1)
     (make-array (array-total-size data2)
                 :element-type (array-element-type data2)
                 :displaced-to data2)
     significant-figures)))

(defmacro assert-sigfig-equal (expected form &rest extras)
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'sigfig-equal))

;;; (NUMBER-EQUAL number1 number2) => true or false
(defun number-equal (number1 number2 &optional (epsilon *epsilon*) type-eq-p)
  "Return true if the numbers are equal within some epsilon,
optionally requiring the types to be identical."
  (and
   (or (not type-eq-p) (eq (type-of number1) (type-of number2)))
   (float-equal (coerce number1 '(complex double-float))
                (coerce number2 '(complex double-float))
                epsilon)))

(defmacro assert-number-equal (expected form &rest extras)
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'number-equal))

;;; (NUMERICAL-EQUAL result1 result2) => true or false
;;;
;;; This is a universal wrapper created by Liam Healy. It is
;;; implemented to support testing in GSLL. The interface is expanded,
;;; but backwards compatible with previous versions.
;;;
(defmethod numerical-equal ((result1 number) (result2 number)
			    &key (test #'number-equal))
  "Return true if the the numbers are equal according to :TEST."
  (funcall test result1 result2))

(defun %sequence-equal (seq1 seq2 test)
  "Return true if the sequences are equal in length and each element
is equal according to :TEST."
  (when (= (length seq1) (length seq2))
    (every (lambda (s1 s2) (numerical-equal s1 s2 :test test))
	   seq1 seq2)))

(defmethod numerical-equal ((result1 list) (result2 list)
			    &key (test #'number-equal))
  "Return true if the lists are equal in length and each element is
equal according to :TEST."
  (%sequence-equal result1 result2 test))

(defmethod numerical-equal ((result1 vector) (result2 vector)
			    &key (test #'number-equal))
  "Return true if the vectors are equal in length and each element is
equal according to :TEST."
  (%sequence-equal result1 result2 test))

(defmethod numerical-equal ((result1 list) (result2 vector)
			    &key (test #'number-equal))
  "Return true if every element of the list is equal to the
corresponding element of the vector."
  (%sequence-equal result1 result2 test))

(defmethod numerical-equal ((result1 vector) (result2 list)
			    &key (test #'number-equal))
  "Return true if every element of the list is equla to the
corresponding element of the vector."
  (%sequence-equal result1 result2 test))

(defmethod numerical-equal ((result1 array) (result2 array)
			    &key (test #'number-equal))
  "Return true if the arrays are equal in dimension and each element
is equal according to :TEST."
  (when (equal (array-dimensions result1) (array-dimensions result2))
    (every test
	   (make-array (array-total-size result1)
		       :element-type (array-element-type result1)
		       :displaced-to result1)
	   (make-array (array-total-size result2)
		       :element-type (array-element-type result2)
		       :displaced-to result2))))

(defmacro assert-numerical-equal (expected form &rest extras)
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'numerical-equal))

;;; FIXME : Audit and move the diagnostic functions to a separate
;;; file.

;;; Diagnostic functions
;;; Failing a unit test is only half the problem.

;;; Sequence errors and the indices.
(defun %sequence-error (sequence1 sequence2 test error-function)
  "Return a sequence of the indice and error between the sequences."
  (let ((n1 nil) (n2 nil)
	(errseq '()))
    (dotimes (index (length sequence1) errseq)
      (setf n1 (elt sequence1 index)
	    n2 (elt sequence2 index))
      (unless (funcall test n1 n2)
	(push (list (1- index) n1 n2 (funcall error-function n1 n2))
	      errseq)))))

(defun sequence-error (sequence1 sequence2 &key
		       (test #'number-equal)
		       (error-function #'relative-error))
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
	(n1 nil) (n2 nil)
	(indices '())
	(errseq '()))
    (dotimes (index (array-total-size array1) errseq)
      (setf indices (%array-indices index dimensions)
	    n1 (apply #'aref array1 indices)
	    n2 (apply #'aref array2 indices))
      (unless (funcall test n1 n2)
	(push (list indices n1 n2 (funcall errfun n1 n2))
	      errseq)))))

(defun array-error (array1 array2 &key
		    (test #'number-equal)
		    (error-function #'relative-error))
  "Return a list of the indices and error between the array elements."
  (cond
    ((not (arrayp array1))
     (error "ARRAY1 is not an array."))
    ((not (arrayp array2))
     (error "ARRAY2 is not an array."))
    ((not (equal (array-dimensions array1) (array-dimensions array2)))
     (error "Arrays are not equal dimensions."))
    (t (%array-error array1 array2 test error-function))))

;;; Floating point data functions
(defun make-2d-list (rows columns &key (initial-element 0))
  "Return a nested list with INITIAL-ELEMENT."
  (mapcar (lambda (x) (make-list columns :initial-element x))
          (make-list rows :initial-element initial-element)))

(defun %complex-float-random (limit &optional (state *random-state*))
  "Return a random complex float number."
  (complex
   (random (realpart limit) state)
   (random (imagpart limit) state)))

(defun %complex-rational-random (limit &optional (state *random-state*))
  "Return a random complex rational number."
  (let ((imaglimit (imagpart limit)))
    (if (< 1 imaglimit)
        (complex
         (random (realpart limit) state)
         ;; Ensure that the imaginary part is not zero.
         (do ((im (random imaglimit state)
                  (random imaglimit state)))
             ((< 0 im) im)))
        (error "Imaginary part must be greater than 1."))))

(defun complex-random (limit &optional (state *random-state*))
  "Return a random complex number. "
  (check-type limit complex)
  (if (typep limit '(complex rational))
      (%complex-rational-random limit state)
      (%complex-float-random limit state)))

(defun make-random-list (size &optional (limit 1.0))
  "Return a list of random numbers."
  (mapcar (if (complexp limit) #'complex-random #'random)
	  (make-list size :initial-element limit)))

(defun make-random-2d-list (rows columns &optional (limit 1.0))
  "Return a nested list of random numbers."
  (mapcar (lambda (x) (make-random-list columns x))
          (make-list rows :initial-element limit)))

(defun make-random-2d-array (rows columns &optional (limit 1.0))
  "Return a 2D array of random numbers."
  (let ((new-array (make-array (list rows columns)
			       :element-type (type-of limit)))
	(random-func (if (complexp limit)
			 #'complex-random
			 #'random)))
    (dotimes (i0 rows new-array)
      (dotimes (i1 columns)
	(setf (aref new-array i0 i1)
	      (funcall random-func limit))))))
