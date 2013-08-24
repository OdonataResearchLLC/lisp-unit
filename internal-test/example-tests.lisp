#|

 LISP-UNIT Example Tests

 Copyright (c) 2010-2012, Thomas M. Hermann
 All rights reserved.

 Redistribution and  use  in  source  and  binary  forms, with or without
 modification, are permitted  provided  that the following conditions are
 met:

   o  Redistributions of  source  code  must  retain  the above copyright
      notice, this list of conditions and the following disclaimer.
   o  Redistributions in binary  form  must reproduce the above copyright
      notice, this list of  conditions  and  the  following disclaimer in
      the  documentation  and/or   other   materials  provided  with  the
      distribution.
   o  The names of the contributors may not be used to endorse or promote
      products derived from this software without  specific prior written
      permission.

 THIS SOFTWARE IS  PROVIDED  BY  THE  COPYRIGHT  HOLDERS AND CONTRIBUTORS
 "AS IS"  AND  ANY  EXPRESS  OR  IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS FOR A
 PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT LIMITED TO,
 PROCUREMENT OF  SUBSTITUTE  GOODS  OR  SERVICES;  LOSS  OF USE, DATA, OR
 PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER  CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER  IN  CONTRACT,  STRICT  LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR  OTHERWISE)  ARISING  IN  ANY  WAY  OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
