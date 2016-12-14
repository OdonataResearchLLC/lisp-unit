#|

  LISP-UNIT Example Tests

  Copyright (c) 2010-2016, Thomas M. Hermann

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

(in-package :lisp-unit)

(defun my-max (x y)
  "Deliberately wrong"
  (declare (ignore y))
  x)

(define-test test-my-max
  ;; Wrong
  (assert-equal 5 (my-max 2 5))
  (assert-equal 5 (my-max 5 2))
  (assert-equal 10 (my-max 10 10))
  (assert-equal 0 (my-max -5 0))
  ;; Error
  (assert-equal 5 (my-max-err 2 5)))

(defun my-sqrt (n)
  "Not really."
  (/ n 2))

(define-test my-sqrt
  (dotimes (i 5)
    (assert-equal i (my-sqrt (* i i)) i)))

(define-test cl-user::my-sqrt
  (dotimes (i 5)
    (assert-equal i (my-sqrt (* i i)) i)))

;;; Expand true or false

(defun my-less-than-function (arg1 arg2)
  (< arg1 arg2))

(define-test expand-t-or-f-function
  (let ((val 0))
    (assert-true (my-less-than-function (incf val) (incf val)))
    (assert-eql 2 val)
    (assert-true (my-less-than-function (incf val) (decf val)))
    (assert-eql 2 val)))

(defmacro my-less-than-macro (arg1 arg2)
  `(< ,arg1 ,arg2))

(define-test expand-t-or-f-macro
  (let ((val 0))
    (assert-true (my-less-than-macro (incf val) (incf val)))
    (assert-eql 2 val)
    (assert-true (my-less-than-macro (incf val) (decf val)))
    (assert-eql 2 val))
  (assert-true (and nil (/ 1 0))))

;;; Macro

(defmacro my-macro (arg1 arg2)
  (let ((g1 (gensym))
        (g2 (gensym)))
    `(let ((,g1 ,arg1)
           (,g2 ,arg2))
       "Start"
       (+ ,g1 ,g2 3))))

(define-test test-macro
  (assert-expands
   (let ((#:G1 A) (#:G2 B)) "Start" (+ #:G1 #:G2 3))
   (my-macro a b)))

;;; Tags

(defun add-integer (integer1 integer2)
  "Add 2 integer numbers"
  (check-type integer1 integer)
  (check-type integer2 integer)
  (+ integer1 integer2))

(defun subtract-integer (integer1 integer2)
  "Subtract 2 integer numbers"
  (check-type integer1 integer)
  (check-type integer2 integer)
  (- integer1 integer2))

(define-test add-integer
  "Test add-integer for values and errors."
  (:tag :add :integer)
  (assert-eql 3 (add-integer 1 2))
  (assert-error 'type-error (add-integer 1.0 2))
  (assert-error 'type-error (add-integer 1 2.0)))

(define-test subtract-integer
  "Test subtract-integer for values and errors."
  (:tag :subtract :integer)
  (assert-eql 1 (subtract-integer 3 2))
  (assert-error 'type-error (subtract-integer 3.0 2))
  (assert-error 'type-error (subtract-integer 2 3.0)))

(defun add-float (float1 float2)
  "Add 2 floating point numbers"
  (check-type float1 float)
  (check-type float2 float)
  (+ float1 float2))

(defun subtract-float (float1 float2)
  "Subtract 2 floating point numbers"
  (check-type float1 float)
  (check-type float2 float)
  (- float1 float2))

(define-test add-float
  "Test add-float for values and errors."
  (:tag :add :float)
  (assert-eql 3.0 (add-float 1.0 2.0))
  (assert-error 'type-error (add-float 1.0 2))
  (assert-error 'type-error (add-float 1 2.0)))

(define-test subtract-float
  "Test subtract-float for values and errors."
  (:tag :subtract :float)
  (assert-eql 1.0 (subtract-float 3.0 2.0))
  (assert-error 'type-error (subtract-float 3.0 2))
  (assert-error 'type-error (subtract-float 2 3.0)))

(defun add-complex (complex1 complex2)
  "Add 2 complex numbers"
  (check-type complex1 complex)
  (check-type complex2 complex)
  (+ complex1 complex2))

(defun subtract-complex (complex1 complex2)
  "Subtract 2 complex numbers"
  (check-type complex1 complex)
  (check-type complex2 complex)
  (- complex1 complex2))

(define-test add-complex
  "Test add-complex for values and errors."
  (:tag :add :complex)
  (assert-eql #C(3 5) (add-complex #C(1 2) #C(2 3)))
  (assert-error 'type-error (add-integer #C(1 2) 3))
  (assert-error 'type-error (add-integer 1 #C(2 3))))

(define-test subtract-complex
  "Test subtract-complex for values and errors."
  (:tag :subtract :complex)
  (assert-eql #C(1 2) (subtract-complex #C(3 5) #C(2 3)))
  (assert-error 'type-error (subtract-integer #C(3 5) 2))
  (assert-error 'type-error (subtract-integer 2 #C(2 3))))
