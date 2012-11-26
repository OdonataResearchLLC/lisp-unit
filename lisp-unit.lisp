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


How to use
----------

1. Read the documentation at:
   https://github.com/OdonataResearchLLC/lisp-unit/wiki

2. Make a file of DEFINE-TEST's. See exercise-tests.lisp for many
examples. If you want, start your test file with (REMOVE-TESTS :ALL)
to clear any previously defined tests.

3. Load this file.

4. (use-package :lisp-unit)

5. Load your code file and your file of tests.

6. Test your code with (RUN-TESTS '(test-name1 test-name2 ...)) or
simply (RUN-TESTS :ALL) to run all defined tests.

A summary of how many tests passed and failed will be printed.

NOTE: Nothing is compiled until RUN-TESTS is expanded. Redefining
functions or even macros does not require reloading any tests.

|#

;;; Packages

(in-package :cl-user)

(defpackage :lisp-unit
  (:use :common-lisp)
  ;; Print parameters
  (:export :*print-summary*
           :*print-failures*
           :*print-errors*)
  ;; Forms for assertions
  (:export :assert-eq
           :assert-eql
           :assert-equal
           :assert-equalp
           :assert-equality
           :assert-prints
           :assert-expands
           :assert-true
           :assert-false
           :assert-error)
  ;; Functions for managing tests
  (:export :define-test
           :list-tests
           :test-code
           :test-documentation
           :remove-tests
           :run-tests
           :use-debugger)
  ;; Functions for managing tags
  (:export :list-tags
           :tagged-tests
           :remove-tags
           :run-tags)
  ;; Functions for reporting test results
  (:export :test-names
           :failed-tests
           :error-tests
           :missing-tests
           :print-failures
           :print-errors
           :summarize-results)
  ;; Utility predicates
  (:export :logically-equal :set-equal))

(in-package :lisp-unit)

;;; Global counters

(defparameter *pass* ()
  "The passed assertion results.")

(defparameter *fail* ()
  "The failed assertion results.")

(defun reset-counters ()
  "Reset the counters to empty lists."
  (setf *pass* () *fail* ()))

;;; Global options

(defparameter *print-summary* nil
  "Print a summary of the pass, fail, and error count if non-nil.")

(defparameter *print-failures* nil
  "Print failure messages if non-NIL.")

(defparameter *print-errors* nil
  "Print error messages if non-NIL.")

(defparameter *use-debugger* nil
  "If not NIL, enter the debugger when an error is encountered in an
assertion.")

(defun use-debugger-p (condition)
  "Debug or ignore errors."
  (cond
   ((eq :ask *use-debugger*)
    (y-or-n-p "~A -- debug?" condition))
   (*use-debugger*)))

(defun use-debugger (&optional (flag t))
  "Use the debugger when testing, or not."
  (setq *use-debugger* flag))

;;; Global unit test database

(defparameter *test-db* (make-hash-table :test #'eq)
  "The unit test database is simply a hash table.")

(defun package-table (package &optional create)
  (cond
   ((gethash (find-package package) *test-db*))
   (create
    (setf (gethash package *test-db*) (make-hash-table)))
   (t (warn "No tests defined for package: ~S" package))))

;;; Global tags database

(defparameter *tag-db* (make-hash-table :test #'eq)
  "The tag database is simply a hash table.")

(defun package-tags (package &optional create)
  "Return the tags DB for the package."
  (cond
   ((gethash (find-package package) *tag-db*))
   (create
    (setf (gethash package *tag-db*) (make-hash-table)))
   (t (warn "No tags defined for package: ~S" package))))

;;; Unit test definition

(defclass unit-test ()
  ((doc
    :type string
    :initarg :doc
    :reader doc)
   (code
    :type list
    :initarg :code
    :reader code))
  (:default-initargs :doc "" :code ())
  (:documentation
   "Organize the unit test documentation and code."))

;;; NOTE: Shamelessly taken from PG's analyze-body
(defun parse-body (body &optional doc tag)
  "Separate the components of the body."
  (let ((item (first body)))
    (cond
     ((and (listp item) (eq :tag (first item)))
      (parse-body (rest body) doc (nconc (rest item) tag)))
     ((and (stringp item) (not doc) (rest body))
      (if tag
          (values doc tag (rest body))
          (parse-body (rest body) doc tag)))
     (t (values doc tag body)))))

(defmacro define-test (name &body body)
  "Store the test in the test database."
  (multiple-value-bind (doc tag code) (parse-body body)
    `(let ((doc (or ,doc (string ',name))))
       (setf
        ;; Unit test
        (gethash ',name (package-table *package* t))
        (make-instance 'unit-test :doc doc :code ',code))
       ;; Tags
       (loop for tag in ',tag do
             (pushnew
              ',name (gethash tag (package-tags *package* t))))
       ;; Return the name of the test
       ',name)))

;;; Manage tests

(defun list-tests (&optional (package *package*))
  "Return a list of the tests in package."
  (let ((table (package-table package)))
    (when table
      (loop for test-name being each hash-key in table
            collect test-name))))

(defun test-documentation (name &optional (package *package*))
  "Return the documentation for the test."
  (let ((unit-test (gethash name (package-table package))))
    (if (null unit-test)
        (warn "No code defined for test ~A in package ~S."
              name package)
        (doc unit-test))))

(defun test-code (name &optional (package *package*))
  "Returns the code stored for the test name."
  (let ((unit-test (gethash name (package-table package))))
    (if (null unit-test)
        (warn "No code defined for test ~A in package ~S."
              name package)
        (code unit-test))))

(defun remove-tests (names &optional (package *package*))
  "Remove individual tests or entire sets."
  (if (eq :all names)
      (if (null package)
          (clrhash *test-db*)
          (progn
            (remhash (find-package package) *test-db*)
            (remhash (find-package package) *tag-db*)))
      (let ((table (package-table package)))
        (unless (null table)
          ;; Remove tests
          (loop for name in names
                always (remhash name table)
                collect name into removed
                finally (return removed))
          ;; Remove tests from tags
          (loop with tags = (package-tags package)
                for tag being each hash-key in tags
                using (hash-value tagged-tests)
                do
                (setf
                 (gethash tag tags)
                 (set-difference tagged-tests names)))))))

;;; Manage tags

(defun %tests-from-all-tags (&optional (package *package*))
  "Return all of the tests that have been tagged."
  (loop for tests being each hash-value in (package-tags package)
        nconc (copy-list tests) into all-tests
        finally (return (delete-duplicates all-tests))))

(defun %tests-from-tags (tags &optional (package *package*))
  "Return the tests associated with the tags."
  (loop with table = (package-tags package)
        for tag in tags
        as tests = (gethash tag table)
        nconc (copy-list tests) into all-tests
        finally (return (delete-duplicates all-tests))))

(defun list-tags (&optional (package *package*))
  "Return a list of the tags in package."
  (let ((tags (package-tags package)))
    (when tags
      (loop for tag being each hash-key in tags collect tag))))

(defun tagged-tests (tags &optional (package *package*))
  "Run the tests associated with the specified tags in package."
  (if (eq :all tags)
      (%tests-from-all-tags package)
      (%tests-from-tags tags package)))

(defun remove-tags (tags &optional (package *package*))
  "Remove individual tags or entire sets."
  (if (eq :all tags)
      (if (null package)
          (clrhash *tag-db*)
          (remhash (find-package package) *tag-db*))
      (let ((table (package-tags package)))
        (unless (null table)
          (loop for tag in tags
                always (remhash tag table)
                collect tag into removed
                finally (return removed))))))

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

(defclass assert-result ()
  ((form
    :initarg :form
    :reader form)
   (actual
    :type list
    :initarg :actual
    :reader actual)
   (expected
    :type list
    :initarg :expected
    :reader expected)
   (extras
    :type list
    :initarg :extras
    :reader extras)
   (test
    :type function
    :initarg :test
    :reader test)
   (passed
    :type boolean
    :reader passed))
  (:documentation
   "Result of the assertion."))

(defmethod initialize-instance :after ((self assert-result)
                                       &rest initargs)
  "Evaluate the actual and expected forms"
  (with-slots (actual expected) self
    (setf
     actual (multiple-value-list (funcall actual))
     expected (multiple-value-list (funcall expected))))
  ;; Generate extras
  (when (slot-boundp self 'extras)
    (setf
     (slot-value self 'extras)
     (funcall (slot-value self 'extras)))))

(defclass equal-result (assert-result)
  ()
  (:documentation
   "Result of an equal assertion type."))

(defmethod initialize-instance :after ((self equal-result)
                                       &rest initargs)
  "Return the result of the equality assertion."
  (with-slots (actual expected test passed) self
    (setf
     passed
     (and
      (<= (length expected) (length actual))
      (every test expected actual)))))

(defclass error-result (assert-result)
  ()
  (:documentation
   "Result of an error assertion type."))

(defmethod initialize-instance :after ((self error-result)
                                       &rest initargs)
  "Evaluate the result."
  (with-slots (actual expected passed) self
    (setf
     passed
     (or
      (eql (car actual) (car expected))
      (typep (car actual) (car expected))))))

(defclass macro-result (assert-result)
  ()
  (:documentation
   "Result of a macro assertion type."))

(defmethod initialize-instance :after ((self macro-result)
                                       &rest initargs)
  "Return the result of the macro expansion."
  (with-slots (actual expected passed) self
    (setf passed (equal (car actual) (car expected)))))

(defclass boolean-result (assert-result)
  ()
  (:documentation
   "Result of a result assertion type."))

(defmethod initialize-instance :after ((self boolean-result)
                                       &rest initargs)
  "Return the result of the assertion."
  (with-slots (actual expected passed) self
    (setf passed (logically-equal (car actual) (car expected)))))

(defclass output-result (assert-result)
  ()
  (:documentation
   "Result of an output assertion type."))

(defmethod initialize-instance :after ((self output-result)
                                       &rest initargs)
  "Return the result of the printed output."
  (with-slots (actual expected passed) self
    (setf
     passed
     (string=
      (string-trim '(#\newline #\return #\space) (car actual))
      (car expected)))))

(defun assert-class (type)
  "Return the class name for the assertion type."
  (ecase type
    (:equal 'equal-result)
    (:error 'error-result)
    (:macro 'macro-result)
    (:result 'boolean-result)
    (:output 'output-result)))

(defun internal-assert
       (type form code-thunk expected-thunk extras test)
  "Perform the assertion and record the results."
  (let ((result
         (make-instance (assert-class type)
                        :form form
                        :actual code-thunk
                        :expected expected-thunk
                        :extras extras
                        :test test)))
    (if (passed result)
        (push result *pass*)
        (push result *fail*))
    ;; Return the result
    (passed result)))

;;; Unit test results

(defclass test-result ()
  ((name
    :type symbol
    :initarg :name
    :reader name)
   (pass
    :type list
    :initarg :pass
    :reader pass)
   (fail
    :type list
    :initarg :fail
    :reader fail)
   (exerr
    :type condition
    :initarg :exerr
    :reader exerr))
  (:default-initargs :exerr nil)
  (:documentation
   "Store the results of the unit test."))

(defun print-summary (test-result)
  "Print a summary of the test result."
  (format t "~&~A: ~S assertions passed, ~S failed"
          (name test-result)
          (length (pass test-result))
          (length (fail test-result)))
  (if (exerr test-result)
      (format t ", and an execution error.")
      (write-char #\.))
  (terpri)
  (terpri))

(defun run-code (code)
  "Run the code to test the assertions."
  (funcall (coerce `(lambda () ,@code) 'function)))

(defun run-test-thunk (name code)
  (let ((*pass* ())
        (*fail* ()))
    (handler-bind
        ((error
          (lambda (condition)
            (if (use-debugger-p condition)
                condition
                (return-from run-test-thunk
                  (make-instance
                   'test-result
                   :name name
                   :pass *pass*
                   :fail *fail*
                   :exerr condition))))))
      (run-code code))
    ;; Return the result count
    (make-instance 'test-result
                   :name name
                   :pass *pass*
                   :fail *fail*)))

;;; Test results database

(defclass test-results-db ()
  ((database
    :type hash-table
    :initform (make-hash-table :test #'eq)
    :reader database)
   (pass
    :type fixnum
    :initform 0
    :accessor pass)
   (fail
    :type fixnum
    :initform 0
    :accessor fail)
   (exerr
    :type fixnum
    :initform 0
    :accessor exerr)
   (failed-tests
    :type list
    :initform ()
    :accessor failed-tests)
   (error-tests
    :type list
    :initform ()
    :accessor error-tests)
   (missing-tests
    :type list
    :initform ()
    :accessor missing-tests))
  (:documentation
   "Store the results of the tests for further evaluation."))

(defmethod print-object ((object test-results-db) stream)
  "Print the summary counts with the object."
  (let ((pass (pass object))
        (fail (fail object))
        (exerr (exerr object)))
    (format
     stream "#<~A Total(~D) Passed(~D) Failed(~D) Errors(~D)>~%"
     (class-name (class-of object))
     (+ pass fail) pass fail exerr)))

(defun test-names (test-results-db)
  "Return a list of the test names in the database."
  (loop for name being each hash-key in (database test-results-db)
        collect name))

(defun record-result (test-name code results)
  "Run the test code and record the result."
  (let ((result (run-test-thunk test-name code)))
    ;; Store the result
    (setf (gethash test-name (database results)) result)
    ;; Count passed tests
    (when (pass result)
      (incf (pass results) (length (pass result))))
    ;; Count failed tests and record the name
    (when (fail result)
      (incf (fail results) (length (fail result)))
      (push test-name (failed-tests results)))
    ;; Count errors and record the name
    (when (exerr result)
      (incf (exerr results))
      (push test-name (error-tests results)))
    ;; Running output
    (when *print-failures* (print-failures result))
    (when *print-errors* (print-errors result))
    (when (or *print-summary* *print-failures* *print-errors*)
      (print-summary result))))

(defun summarize-results (results)
  "Print a summary of all results."
  (let ((pass (pass results))
        (fail (fail results)))
    (format t "~&Unit Test Summary~%")
    (format t " | ~D assertions total~%" (+ pass fail))
    (format t " | ~D passed~%" pass)
    (format t " | ~D failed~%" fail)
    (format t " | ~D execution errors~%" (exerr results))
    (format t " | ~D missing tests~2%"
            (length (missing-tests results)))))

;;; Run the tests

(defun %run-all-thunks (&optional (package *package*))
  "Run all of the test thunks in the package."
  (loop
   with results = (make-instance 'test-results-db)
   for test-name being each hash-key in (package-table package)
   using (hash-value unit-test)
   if unit-test do
   (record-result test-name (code unit-test) results)
   else do
   (push test-name (missing-tests results))
   ;; Summarize and return the test results
   finally
   (summarize-results results)
   (return results)))

(defun %run-thunks (test-names &optional (package *package*))
  "Run the list of test thunks in the package."
  (loop
   with table = (package-table package)
   and results = (make-instance 'test-results-db)
   for test-name in test-names
   as unit-test = (gethash test-name table)
   if unit-test do
   (record-result test-name (code unit-test) results)
   else do
   (push test-name (missing-tests results))
   finally
   (summarize-results results)
   (return results)))

(defun run-tests (test-names &optional (package *package*))
  "Run the specified tests in package."
  (reset-counters)
  (if (eq :all test-names)
      (%run-all-thunks package)
      (%run-thunks test-names package)))

(defun run-tags (tags &optional (package *package*))
  "Run the tests associated with the specified tags in package."
  (reset-counters)
  (%run-thunks (tagged-tests tags package) package))

;;; Print failures

(defgeneric print-failures (result)
  (:documentation
   "Report the results of the failed assertion."))

(defmethod print-failures :around ((result assert-result))
  "Failure header and footer output."
  (format t "~& | Failed Form: ~S" (form result))
  (call-next-method)
  (when (extras result)
    (format t "~{~& | ~S => ~S~}~%" (extras result)))
  (format t "~& |~%")
  (class-name (class-of result)))

(defmethod print-failures ((result assert-result))
  (format t "~& | Expected ~{~S~^; ~} " (expected result))
  (format t "~<~% | ~:;but saw ~{~S~^; ~}~>" (actual result)))

(defmethod print-failures ((result error-result))
  (format t "~& | ~@[Should have signalled ~{~S~^; ~} but saw~]"
          (expected result))
  (format t " ~{~S~^; ~}" (actual result)))

(defmethod print-failures ((result macro-result))
  (format t "~& | Should have expanded to ~{~S~^; ~} "
          (expected result))
  (format t "~<~%~:;but saw ~{~S~^; ~}~>" (actual result)))

(defmethod print-failures ((result output-result))
  (format t "~& | Should have printed ~{~S~^; ~} "
          (expected result))
  (format t "~<~%~:;but saw ~{~S~^; ~}~>"
          (actual result)))

(defmethod print-failures ((result test-result))
  "Print the failed assertions in the unit test."
  (loop for fail in (fail result) do
        (print-failures fail)))

(defmethod print-failures ((results test-results-db))
  "Print all of the failure tests."
  (loop with db = (database results)
        for test in (failed-tests results)
        as result = (gethash test db)
        do
        (print-failures result)
        (print-summary result)))

;;; Print errors

(defgeneric print-errors (result)
  (:documentation
   "Print the error condition."))

(defmethod print-errors ((result test-result))
  "Print the error condition."
  (let ((exerr (exerr result))
        (*print-escape* nil))
    (when exerr
      (format t "~& | Execution error:~% | ~W" exerr)
      (format t "~& |~%"))))

(defmethod print-errors ((results test-results-db))
  "Print all of the error tests."
  (loop with db = (database results)
        for test in (error-tests results)
        as result = (gethash test db)
        do
        (print-errors result)
        (print-summary result)))

;;; Useful equality predicates for tests

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
