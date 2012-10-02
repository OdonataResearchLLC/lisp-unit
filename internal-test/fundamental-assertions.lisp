#|

 LISP-UNIT Internal Tests

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

;;; Internal utility functions

(defun %expansion-equal (form1 form2)
  "Descend into the forms checking for equality."
  (let ((item1 (first form1))
        (item2 (first form2)))
    (cond
     ((and (null item1) (null item2)))
     ((and (listp item1) (listp item2))
      (and (%expansion-equal item1 item2)
           (%expansion-equal (rest form1) (rest form2))))
     ((and (symbolp item1) (symbolp item2))
      (and (string= (symbol-name item1) (symbol-name item2))
           (%expansion-equal (rest form1) (rest form2))))
     (t nil))))

(defun expansion-equal (macro-form expansion)
  "MACROEXPAND-1 the macro-form and compare with the expansion."
  (let ((*gensym-counter* 1))
    (%expansion-equal (macroexpand-1 macro-form) expansion)))

(defun test-macro-expansions (expansions)
  "Test each fundamental assertion and report the results."
  (loop for (assertion macro-form expansion) in expansions collect
        (list assertion (expansion-equal macro-form expansion))))

;;; Expansions

(defvar *expand-assert-expansions*
  '(("EXPAND-ASSERT-BASIC"
     (expand-assert
      :equal form form expected (extra1 extra2) :test #'eq)
     (INTERNAL-ASSERT :EQUAL
                      (QUOTE FORM)
                      (LAMBDA NIL FORM)
                      (LAMBDA NIL EXPECTED)
                      (EXPAND-EXTRAS (EXTRA1 EXTRA2))
                      (FUNCTION EQ)))
    ("EXPAND-ASSERT-ERROR"
     (expand-assert
      :error form (expand-error-form form) condition (extra1 extra2))
     (INTERNAL-ASSERT :ERROR
                      (QUOTE FORM)
                      (LAMBDA NIL (HANDLER-CASE FORM (CONDITION (ERROR) ERROR)))
                      (LAMBDA NIL (QUOTE CONDITION))
                      (EXPAND-EXTRAS (EXTRA1 EXTRA2))
                      (FUNCTION EQL)))
    ("EXPAND-ASSERT-MACRO"
     (expand-assert
      :macro form
      (expand-macro-form form nil)
      expansion (extra1 extra2))
     (INTERNAL-ASSERT :MACRO
                      (QUOTE FORM)
                      (LAMBDA NIL (MACROEXPAND-1 (QUOTE FORM) NIL))
                      (LAMBDA NIL EXPANSION)
                      (EXPAND-EXTRAS (EXTRA1 EXTRA2))
                      (FUNCTION EQL)))
    ("EXPAND-ASSERTS-PRINT"
     (expand-assert
      :output form (expand-output-form form) output (extra1 extra2))
     (INTERNAL-ASSERT :OUTPUT
                      (QUOTE FORM)
                      (LAMBDA NIL
                        (LET* ((#:G1 (MAKE-STRING-OUTPUT-STREAM))
                               (*STANDARD-OUTPUT* (MAKE-BROADCAST-STREAM
                                                   *STANDARD-OUTPUT* #:G1)))
                          FORM
                          (GET-OUTPUT-STREAM-STRING #:G1)))
                      (LAMBDA NIL OUTPUT)
                      (EXPAND-EXTRAS (EXTRA1 EXTRA2))
                      (FUNCTION EQL))))
  "The correct expansions for the expand-assert macro.")

(defvar *expansion-macros*
  '(("EXPAND-ERROR-FORM"
     (expand-error-form form)
     (HANDLER-CASE FORM (CONDITION (ERROR) ERROR)))
    ("EXPAND-OUTPUT-FORM"
     (expand-output-form form)
     (LET* ((#:G1 (MAKE-STRING-OUTPUT-STREAM))
            (*STANDARD-OUTPUT*
             (MAKE-BROADCAST-STREAM *STANDARD-OUTPUT* #:G1)))
       FORM
       (GET-OUTPUT-STREAM-STRING #:G1)))
    ("EXPAND-MACRO-FORM"
     (expand-macro-form form env)
     (MACROEXPAND-1 'FORM ENV))
    ("EXPAND-EXTRAS"
     (expand-extras (extra1 extra2))
     (LAMBDA NIL (LIST (QUOTE EXTRA1) EXTRA1 (QUOTE EXTRA2) EXTRA2))))
  "The correct expansions for macros that expand forms.")

(defvar *fundamental-assertion-expansions*
  '(("ASSERT-EQ"
     (assert-eq expected form extra1 extra2)
     (EXPAND-ASSERT
      :EQUAL FORM FORM EXPECTED (EXTRA1 EXTRA2) :TEST (FUNCTION EQ)))
    ("ASSERT-EQL"
     (assert-eql expected form extra1 extra2)
     (EXPAND-ASSERT
      :EQUAL FORM FORM EXPECTED (EXTRA1 EXTRA2) :TEST (FUNCTION EQL)))
    ("ASSERT-EQUAL"
     (assert-equal expected form extra1 extra2)
     (EXPAND-ASSERT
      :EQUAL FORM FORM EXPECTED (EXTRA1 EXTRA2) :TEST (FUNCTION EQUAL)))
    ("ASSERT-EQUALP"
     (assert-equalp expected form extra1 extra2)
     (EXPAND-ASSERT
      :EQUAL FORM FORM EXPECTED (EXTRA1 EXTRA2) :TEST (FUNCTION EQUALP)))
    ("ASSERT-ERROR"
     (assert-error 'condition form extra1 extra2)
     (EXPAND-ASSERT
      :ERROR FORM (EXPAND-ERROR-FORM FORM) 'CONDITION (EXTRA1 EXTRA2)))
    ("ASSERT-EXPANDS"
     (assert-expands expansion form extra1 extra2)
     (EXPAND-ASSERT
      :MACRO FORM (EXPAND-MACRO-FORM FORM NIL) EXPANSION (EXTRA1 EXTRA2)))
    ("ASSERT-FALSE"
     (assert-false form extra1 extra2)
     (EXPAND-ASSERT :RESULT FORM FORM NIL (EXTRA1 EXTRA2)))
    ("ASSERT-EQUALITY"
     (assert-equality test expected form extra1 extra2)
     (EXPAND-ASSERT
      :EQUAL FORM FORM EXPECTED (EXTRA1 EXTRA2) :TEST TEST))
    ("ASSERT-PRINTS"
     (assert-prints output form extra1 extra2)
     (EXPAND-ASSERT
      :OUTPUT FORM (expand-output-form form) OUTPUT (EXTRA1 EXTRA2)))
    ("ASSERT-TRUE"
     (assert-true form extra1 extra2)
     (EXPAND-ASSERT :RESULT FORM FORM T (EXTRA1 EXTRA2))))
  "The correct expansions for the fundamental assertions.")
