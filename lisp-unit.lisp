;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
Copyright (c) 2004-2005 Christopher K. Riesbeck

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


;;; A test suite package, modelled after JUnit.
;;; Author: Chris Riesbeck
;;; 
;;; Update history:
;;;

#|

How to use
----------

1. Read the documentation in lisp-unit.html.

2. Make a file of DEFINE-TEST's. See exercise-tests.lisp for many
examples. If you want, start your test file with (REMOVE-TESTS) to
clear any previously defined tests.

2. Load this file.

2. (use-package :lisp-unit)

3. Load your code file and your file of tests.

4. Test your code with (RUN-TESTS test-name1 test-name2 ...) -- no quotes! --
or simply (RUN-TESTS) to run all defined tests.

A summary of how many tests passed and failed will be printed,
with details on the failures.

Note: Nothing is compiled until RUN-TESTS is expanded. Redefining
functions or even macros does not require reloading any tests.

For more information, see lisp-unit.html. 

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :lisp-unit
  (:use :common-lisp)
  ;; Forms for assertions
  (:export
   :assert-eq :assert-eql :assert-equal :assert-equalp
   :assert-equality :assert-prints :assert-expands
   :assert-true :assert-false :assert-error)
  ;; Functions for managing tests
  (:export
   :define-test
   :get-tests :get-test-code
   :remove-tests :remove-all-tests
   :run-tests :run-all-tests
   :use-debugger
   :with-test-listener)
  ;; Utility predicates
  (:export
   :logically-equal :set-equal))

(in-package :lisp-unit)

;;; Global counters

(defparameter *pass* 0
  "The number of passed assertions.")

(defparameter *fail* 0
  "The number of failed assertions.")

;;; Global options

(defparameter *report-results* nil
  "If not NIL, report the results of the assertions.")

;;; Global unit test database

(defparameter *test-db* (make-hash-table :test #'eq)
  "The unit test database is simply a hash table.")

(defun package-table (package &optional create)
  (cond
   ((gethash (find-package package) *test-db*))
   (create
    (setf (gethash package *test-db*) (make-hash-table)))))

(defmacro define-test (name &body body)
  "Store the test in the test database."
  `(progn
     (setf
      (gethash ',name (package-table *package* t))
      ',body)
     ;; Return the name of the test
     ',name))

;;; Assert macros

(defmacro assert-eq (expected form &rest extras)
  "Assert whether expected and form are EQ."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'eq))

(defmacro assert-eql (expected form &rest extras)
  "Assert whether expected and form are EQL."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'eql))

(defmacro assert-equal (expected form &rest extras)
  "Assert whether expected and form are EQUAL."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'equal))

(defmacro assert-equalp (expected form &rest extras)
  "Assert whether expected and form are EQUALP."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'equalp))

(defmacro assert-error (condition form &rest extras)
  "Assert whether form signals condition."
  `(expand-assert :error ,form (expand-error-form ,form)
                  ,condition ,extras))

(defmacro assert-expands (expansion form &rest extras)
  "Assert whether form expands to expansion."
  `(expand-assert :macro ,form 
                  (expand-macro-form ,form nil)
                  ,expansion ,extras))

(defmacro assert-false (form &rest extras)
  "Assert whether the form is false."
  `(expand-assert :result ,form ,form nil ,extras))

(defmacro assert-equality (test expected form &rest extras)
  "Assert whether expected and form are equal according to test."
  `(expand-assert :equal ,form ,form ,expected ,extras :test ,test))

(defmacro assert-prints (output form &rest extras)
  "Assert whether printing the form generates the output."
  `(expand-assert :output ,form (expand-output-form ,form)
                  ,output ,extras))

(defmacro assert-true (form &rest extras)
  "Assert whether the form is true."
  `(expand-assert :result ,form ,form t ,extras))

(defmacro expand-assert (type form body expected extras &key (test '#'eql))
  "Expand the assertion to the internal format."
  `(internal-assert ,type ',form
                    (lambda () ,body)
                    (lambda () ,expected)
                    (expand-extras ,extras)
                    ,test))

(defmacro expand-error-form (form)
  "Wrap the error assertion in HANDLER-CASE."
  `(handler-case ,form
     (condition (error) error)))

(defmacro expand-output-form (form)
  "Capture the output of the form in a string."
  (let ((out (gensym)))
    `(let* ((,out (make-string-output-stream))
            (*standard-output*
             (make-broadcast-stream *standard-output* ,out)))
       ,form
       (get-output-stream-string ,out))))

(defmacro expand-macro-form (form env)
  "Expand the macro form once."
  `(macroexpand-1 ',form ,env))

(defmacro expand-extras (extras)
  "Expand extra forms."
  `(lambda ()
     (list ,@(mapcan (lambda (form) (list `',form form)) extras))))

(defun internal-assert
       (type form code-thunk expected-thunk extras test)
  (let ((expected (multiple-value-list (funcall expected-thunk)))
        (actual (multiple-value-list (funcall code-thunk)))
        (passed nil))
    ;; Count the assertion
    (if (setq passed (test-passed-p type expected actual test))
        (incf *pass*)
        (incf *fail*))
    ;; Report the assertion
    (when (and *report-results* (not passed))
      (report-failure type form expected actual extras))
    ;; Return the result
    passed))

;;; Test passed predicate.

(defgeneric test-passed-p (type expected actual test)
  (:documentation
   "Return the result of the test."))

(defmethod test-passed-p ((type (eql :error)) expected actual test)
  "Return the result of the error assertion."
  (or
   (eql (car actual) (car expected))
   (typep (car actual) (car expected))))

(defmethod test-passed-p ((type (eql :equal)) expected actual test)
  "Return the result of the equality assertion."
  (and
   (<= (length expected) (length actual))
   (every test expected actual)))

(defmethod test-passed-p ((type (eql :macro)) expected actual test)
  "Return the result of the macro expansion."
  (equal (car actual) (car expected)))

(defmethod test-passed-p ((type (eql :output)) expected actual test)
  "Return the result of the printed output."
  (string=
   (string-trim '(#\newline #\return #\space) (car actual))
   (car expected)))

(defmethod test-passed-p ((type (eql :result)) expected actual test)
  "Return the result of the assertion."
  (logically-equal (car actual) (car expected)))

;;; Failure control strings for reports.

(defgeneric failure-control-string (type)
  (:method (type)
   "~&Expected ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
  (:documentation
   "Return the FORMAT control string for the failure type."))

(defmethod failure-control-string ((type (eql :error)))
  "~&~@[Should have signalled ~{~S~^; ~} but saw~] ~{~S~^; ~}")

(defmethod failure-control-string ((type (eql :macro)))
  "~&Should have expanded to ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")

(defmethod failure-control-string ((type (eql :output)))
  "~&Should have printed ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")

;;; Reports

(defun report-failure (type form expected actual extras)
  "Report the details of the failure assertion."
  (format t "Failed Form: ~S" form)
  (format t (failure-control-string type) expected actual)
  (when extras
    (format t "~{~&   ~S => ~S~}~%" (funcall extras)))
  type)

(defun report-summary (name pass fail &optional exerr)
  (format t "~&~A: ~S assertions passed," name pass)
  (format t " ~S failed~@[, ~S execution errors~]." fail exerr))

;;; RUN-TESTS

(defclass test-results ()
  ((test-names
    :type list
    :initarg :test-names
    :reader test-names)
   (successful-assertions
    :type fixnum
    :initform 0
    :accessor successful-assertions)
   (failed-assertions
    :type fixnum
    :initform 0
    :accessor failed-assertions)
   (execution-errors
    :type fixnum
    :initform 0
    :accessor execution-errors)
   (failed-tests
    :type list
    :initform ()
    :reader failed-tests)
   (error-tests
    :type list
    :initform ()
    :reader error-tests)
   (missing-tests
    :type list
    :initform ()
    :reader missing-tests))
  (:default-initargs :test-names ())
  (:documentation
   "Store the results of the tests for further evaluation."))

(defmacro run-code (code)
  "Wrap the code in a lambda and FUNCALL it."
  `(funcall (lambda () ,@code)))

(defun use-debugger-p (e)
  (and *use-debugger*
       (or (not (eql *use-debugger* :ask))
           (y-or-n-p "~A -- debug?" e))))

(defun run-test-thunk (code)
  (let ((*pass* 0)
        (*fail* 0))
    (declare (special *pass* *fail*))
    (handler-case (run-code code)
      (error (condition)
        (if (use-debugger-p condition)
            condition
            (return (values *pass* *fail* :error)))))
    ;; Return the result count
    (values *pass* *fail* nil)))

(defun %run-all-thunks (&optional (package *package*))
  "Run all of the test thunks in the package."
  (loop
   with results = (make-instance 'test-results)
   for test-name being each hash-key in (package-table package)
   using (hash-value code)
   if code do
   (multiple-value-bind (pass fail exerr)
       (run-test-thunk test-name code)
     (push test-name (test-names results))
     ;; Count passed tests
     (when (plusp pass)
       (incf (successful-assertions results) pass))
     ;; Count failed tests and record name
     (when (plusp fail)
       (incf (failed-assertions results) fail)
       (push test-name (failed-tests results)))
     ;; Count errors and record name
     (when (eq :error exerr)
       (incf (execution-errors results))
       (push test-name (error-tests results))))
   else do
   (push test-name (missing-tests results))
   ;; Return the test results
   finally (return results)))

(defun run-tests (test-names &optional (package *package*))
  "Run the specified tests in package."
  (if (eq :all test-names)
      (%run-all-thunks package)
      (%run-thunks test-names package)))

(defun get-test-thunks (names &optional (package *package*))
  (loop for name in names collect
        (get-test-thunk name package)))

(defun get-test-thunk (name package)
  (assert (get-test-code name package) (name package)
    "No test defined for ~S in package ~S" name package)
  (list name (coerce `(lambda () ,@(get-test-code name)) 'function)))

(defun use-debugger (&optional (flag t))
  (setq *use-debugger* flag))

(defmacro with-test-listener (listener &body body)
  `(let ((*test-listener* #',listener)) ,@body))

;;; RUN-TESTS support

(defun run-test-thunks (test-thunks)
  (unless (null test-thunks)
    (let ((total-test-count 0)
          (total-pass-count 0)
          (total-error-count 0))
      (dolist (test-thunk test-thunks)
        (multiple-value-bind (test-count pass-count error-count)
            (run-test-thunk (car test-thunk) (cadr test-thunk))
          (incf total-test-count test-count)
          (incf total-pass-count pass-count)
          (incf total-error-count error-count)))
      (unless (null (cdr test-thunks))
        (show-summary 'total total-test-count total-pass-count total-error-count))
      (values))))

(defun run-test-thunk (*test-name* thunk)
  (if (null thunk)
      (format t "~&    Test ~S not found" *test-name*)
      (prog ((*test-count* 0)
             (*pass-count* 0)
             (error-count 0))
        (handler-bind 
            ((error (lambda (e)
                      (let ((*print-escape* nil))
                        (setq error-count 1)         
                        (format t "~&    ~S: ~W" *test-name* e))
                      (if (use-debugger-p e) e (go exit)))))
          (funcall thunk)
          (show-summary *test-name* *test-count* *pass-count*))
        exit
        (return (values *test-count* *pass-count* error-count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-test-code (name &optional (package *package*))
  (let ((table (get-package-table package)))
    (unless (null table)
      (code (gethash name table)))))

(defun get-tests (&optional (package *package*))
  (loop for key being each hash-key in (get-package-table package)
        collect key))

(defun remove-tests (names &optional (package *package*))
  (let ((table (get-package-table package)))
    (unless (null table)
      (if (eq :all names)
          (clrhash table)
          (loop for name in names always
                (remhash name table))))))

(defun remove-all-tests (&optional (package *package*))
  (if (null package)
      (clrhash *tests*)
      (remhash (find-package package) *tests*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DEFINE-TEST support

;;; ASSERTION support

(defclass test-results ()
  ((tests
    :type list
    :initarg :tests
    :reader tests)
   (total-assertions
    :type fixnum
    :initform 0
    :accessor total-assertions)
   (successful-assertions
    :type fixnum
    :initform 0
    :accessor successful-assertions)
   (failed-assertions
    :type fixnum
    :initform 0
    :accessor failed-assertions)
   (execution-errors
    :type fixnum
    :initform 0
    :accessor execution-errors)
   (failed-tests
    :type list
    :initform ()
    :reader failed-tests)
   (error-tests
    :type list
    :initform ()
    :reader error-tests))
  (:documentation
   "Store the results of the tests for further evaluation."))

(defun record-result (passed type form expected actual extras)
  (funcall (or *test-listener* 'default-listener)
           passed type *test-name* form expected actual 
           (and extras (funcall extras))
           *test-count* *pass-count*))

(defun default-listener
       (passed type name form expected actual extras test-count pass-count)
  (declare (ignore test-count pass-count))
  (unless passed
    (show-failure type (get-failure-message type)
                  name form expected actual extras)))

;;; OUTPUT support

(defun collect-form-values (form values)
  (mapcan (lambda (form-arg value)
            (if (constantp form-arg)
                nil
                (list form-arg value)))
          (cdr form)
          values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful equality predicates for tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (LOGICALLY-EQUAL x y) => true or false
;;;   Return true if x and y both false or both true
(defun logically-equal (x y)
  (eql (not x) (not y)))

;;; (SET-EQUAL l1 l2 :test) => true or false
;;;   Return true if every element of l1 is an element of l2
;;;   and vice versa.
(defun set-equal (l1 l2 &key (test #'equal))
  (and (listp l1)
       (listp l2)
       (subsetp l1 l2 :test test)
       (subsetp l2 l1 :test test)))

(pushnew :lisp-unit common-lisp:*features*)
