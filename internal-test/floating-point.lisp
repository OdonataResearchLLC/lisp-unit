#|

 LISP-UNIT Floating Point Tests

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

;;; List norms

(define-test %norm-list
  "Internal test of %norm on lists."
  (:tag :norm)
  ;; Taxicab norm
  (assert-rational-equal
   36 (%norm '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1))
  (assert-float-equal
   19.535658
   (%norm
    '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0))
    1))
  ;; Euclidean norm
  (assert-float-equal
   12.083046
   (%norm '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 2))
  (assert-float-equal
   8.0
   (%norm
    '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0)) 2))
  ;; P-norm
  (let ((data '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
        (zdata '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
                 #C(-2 3) #C(-3 1) #C(-1 0))))
    (assert-float-equal 8.732892 (%norm data 3))
    (assert-float-equal 6.064035 (%norm zdata 3)))
  ;; Infinity norm
  (assert-rational-equal
   6 (%norm
      '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5)
      :infinity))
  (assert-float-equal
   4.0 (%norm
        '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
          #C(-2 3) #C(-3 1) #C(-1 0))
        :infinity)))

;;; Vector norms

(define-test %norm-vector
  "Internal test of %norm on vectors"
  (:tag :norm)
  ;; Taxicab norm
  (assert-rational-equal
   36 (%norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1))
  (assert-float-equal
   19.535658
   (%norm
    #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0))
    1))
  ;; Euclidean norm
  (assert-float-equal
   12.083046
   (%norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 2))
  (assert-float-equal
   8.0
   (%norm
    #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0))
    2))
  ;; P-norm
  (let ((data #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
        (zdata #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
                 #C(-2 3) #C(-3 1) #C(-1 0))))
    (assert-float-equal 8.732892 (%norm data 3))
    (assert-float-equal 6.064035 (%norm zdata 3)))
  ;; Infinity norm
  (assert-rational-equal
   6 (%norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) :infinity))
  (assert-float-equal
   4.0 (%norm
        #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
          #C(-2 3) #C(-3 1) #C(-1 0))
        :infinity)))
