#|

 LISP-UNIT Internal Tests

 Copyright (c) 2010-2011, Thomas M. Hermann
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

(defvar *fundamental-assertion-expansions*
  '(("ASSERT-EQ"
     (assert-eq expected form extra1 extra2)
     (INTERNAL-ASSERT :EQUAL
                      (QUOTE FORM)
                      (LAMBDA NIL FORM)
                      (LAMBDA NIL EXPECTED)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQ)))
    ("ASSERT-EQL"
     (assert-eql expected form extra1 extra2)
     (INTERNAL-ASSERT :EQUAL
                      (QUOTE FORM)
                      (LAMBDA NIL FORM)
                      (LAMBDA NIL EXPECTED)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQL)))
    ("ASSERT-EQUAL"
     (assert-equal expected form extra1 extra2)
     (INTERNAL-ASSERT :EQUAL
                      (QUOTE FORM)
                      (LAMBDA NIL FORM)
                      (LAMBDA NIL EXPECTED)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQUAL)))
    ("ASSERT-EQUALP"
     (assert-equalp expected form extra1 extra2)
     (INTERNAL-ASSERT :EQUAL
                      (QUOTE FORM)
                      (LAMBDA NIL FORM)
                      (LAMBDA NIL EXPECTED)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQUALP)))
    ("ASSERT-ERROR"
     (assert-error 'condition form extra1 extra2)
     (INTERNAL-ASSERT :ERROR
                      (QUOTE FORM)
                      (LAMBDA NIL (HANDLER-CASE FORM (CONDITION (ERROR) ERROR)))
                      (LAMBDA NIL (QUOTE CONDITION))
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQL)))
    ("ASSERT-EXPANDS"
     (assert-expands expansion form extra1 extra2)
     (INTERNAL-ASSERT :MACRO
                      (QUOTE FORM)
                      (LAMBDA NIL (MACROEXPAND-1 (QUOTE FORM) NIL))
                      (LAMBDA NIL EXPANSION)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQL)))
    ("ASSERT-FALSE"
     (assert-false form extra1 extra2)
     (INTERNAL-ASSERT :RESULT
                      (QUOTE FORM)
                      (LAMBDA NIL FORM)
                      (LAMBDA NIL NIL)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQL)))
    ("ASSERT-EQUALITY"
     (assert-equality test expected form extra1 extra2)
     (INTERNAL-ASSERT :EQUAL
                      (QUOTE FORM)
                      (LAMBDA NIL FORM)
                      (LAMBDA NIL EXPECTED)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      TEST))
    ("ASSERT-PRINTS"
     (assert-prints output form extra1 extra2)
     (INTERNAL-ASSERT :OUTPUT
                      (QUOTE FORM)
                      (LAMBDA NIL
                        (LET* ((#:G1 (MAKE-STRING-OUTPUT-STREAM))
                               (*STANDARD-OUTPUT* (MAKE-BROADCAST-STREAM
                                                   *STANDARD-OUTPUT* #:G1)))
                          FORM
                          (GET-OUTPUT-STREAM-STRING #:G1)))
                      (LAMBDA NIL OUTPUT)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQL)))
    ("ASSERT-TRUE"
     (assert-true form extra1 extra2)
     (INTERNAL-ASSERT :RESULT
                      (QUOTE FORM)
                      (LAMBDA NIL FORM)
                      (LAMBDA NIL T)
                      (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1
                                        (QUOTE EXTRA2) EXTRA2))
                      (FUNCTION EQL))))
  "The correct expansions for the fundamental assertions.")

(defun expansion-equal (form1 form2)
  "Descend into the forms checking for equality."
  (let ((item1 (first form1))
        (item2 (first form2)))
    (cond
     ((and (null item1) (null item2)))
     ((and (listp item1) (listp item2))
      (and (expansion-equal item1 item2)
           (expansion-equal (rest form1) (rest form2))))
     ((and (symbolp item1) (symbolp item2))
      (and (string= (symbol-name item1) (symbol-name item2))
           (expansion-equal (rest form1) (rest form2))))
     (t nil))))

(defun test-fundamental-assertions ()
  "Test each fundamental assertion and report the results."
  (loop for (assertion macro-form expansion)
        in *fundamental-assertion-expansions*
        as *gensym-counter* of-type number = 1
        collect
        (list
         assertion
         (expansion-equal
          (macroexpand macro-form) expansion))))
