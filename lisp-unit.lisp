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
  ;; Functions for extensibility via signals
  (:export :signal-results
           :test-run-complete
           :results)
  ;; Utility predicates
  (:export :logically-equal :set-equal))

(in-package :lisp-unit)

;;; Global counters

(defparameter *pass* 0
  "The passed assertion results.")

(defparameter *fail* ()
  "The failed assertion results.")

(defun reset-counters ()
  "Reset the counters to empty lists."
  (setf *pass* 0 *fail* ()))

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

(defparameter *signal-results* nil
  "Signal the result if non NIL.")

(defun use-debugger-p (condition)
  "Debug or ignore errors."
  (cond
   ((eq :ask *use-debugger*)
    (y-or-n-p "~A -- debug?" condition))
   (*use-debugger*)))

(defun use-debugger (&optional (flag t))
  "Use the debugger when testing, or not."
  (setq *use-debugger* flag))

(defun signal-results (&optional (flag t))
  "Signal the results for extensibility."
  (setq *signal-results* flag))

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
  (let ((qname (gensym "NAME-")))
    (multiple-value-bind (doc tag code) (parse-body body)
      `(let* ((,qname ',name)
              (doc (or ,doc (string ,qname))))
         (setf
          ;; Unit test
          (gethash ,qname (package-table *package* t))
          (make-instance 'unit-test :doc doc :code ',code))
         ;; Tags
         (loop for tag in ',tag do
               (pushnew
                ,qname (gethash tag (package-tags *package* t))))
         ;; Return the name of the test
         ,qname))))

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
                unless (remhash name table) do
                (warn "No test ~A in package ~A to remove."
                      name (package-name package)))
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
        if (null tests) do (warn "No tests tagged with ~S." tag)
        else nconc (copy-list tests) into all-tests
        finally (return (delete-duplicates all-tests))))

(defun list-tags (&optional (package *package*))
  "Return a list of the tags in package."
  (let ((tags (package-tags package)))
    (when tags
      (loop for tag being each hash-key in tags collect tag))))

(defun tagged-tests (tags &optional (package *package*))
  "Return a list of the tests associated with the tags."
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
                unless (remhash tag table) do
                (warn "No tag ~A in package ~A to remove."
                      tag (package-name package)))))))

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
                  ',expansion ,extras))

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
  `(let ((*gensym-counter* 1))
     (macroexpand-1 ',form ,env)))

(defmacro expand-extras (extras)
  "Expand extra forms."
  `(lambda ()
     (list ,@(mapcan (lambda (form) (list `',form form)) extras))))

(defclass failure-result ()
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
    :reader test))
  (:documentation
   "Failure details of the assertion."))

(defclass equal-result (failure-result)
  ()
  (:documentation
   "Result of a failed equal assertion."))

(defun equal-result (test expected actual)
  "Return the result of an equal assertion."
  (and
   (<= (length expected) (length actual))
   (every test expected actual)))

(defclass error-result (failure-result)
  ()
  (:documentation
   "Result of a failed error assertion."))

(defun error-result (test expected actual)
  "Return the result of an error assertion."
  (declare (ignore test))
  (or
   (eql (car actual) (car expected))
   (typep (car actual) (car expected))))

(defclass macro-result (failure-result)
  ()
  (:documentation
   "Result of a failed macro expansion assertion."))

(defun %expansion-equal (form1 form2)
  "Descend into the forms checking for equality."
  (let ((item1 (first form1))
        (item2 (first form2)))
    (cond
     ((and (null item1) (null item2)))
     ((and (listp item1) (listp item2))
      (and
       (%expansion-equal item1 item2)
       (%expansion-equal (rest form1) (rest form2))))
     ((and (symbolp item1) (symbolp item2))
      (and
       (string= (symbol-name item1) (symbol-name item2))
       (%expansion-equal (rest form1) (rest form2))))
     (t (and
         (equal item1 item2)
         (%expansion-equal (rest form1) (rest form2)))))))

(defun macro-result (test expected actual)
  "Return the result of a macro assertion."
  (declare (ignore test))
  (%expansion-equal (first expected) (first actual)))

(defclass boolean-result (failure-result)
  ()
  (:documentation
   "Result of a failed boolean assertion."))

(defun boolean-result (test expected actual)
  "Return the result of a result assertion."
  (declare (ignore test))
  (logically-equal (car actual) (car expected)))

(defclass output-result (failure-result)
  ()
  (:documentation
   "Result of a failed output assertion."))

(defun output-result (test expected actual)
  "Return the result of an output assertion."
  (declare (ignore test))
  (string=
   (string-trim '(#\newline #\return #\space) (car actual))
   (car expected)))

(defun assert-function (type)
  "Return the function for the assertion type."
  (ecase type
    (:equal #'equal-result)
    (:error #'error-result)
    (:macro #'macro-result)
    (:result #'boolean-result)
    (:output #'output-result)))

(defun assert-class (type)
  "Return the class for the assertion type."
  (ecase type
    (:equal 'equal-result)
    (:error 'error-result)
    (:macro 'macro-result)
    (:result 'boolean-result)
    (:output 'output-result)))

(defun internal-assert
       (type form code-thunk expected-thunk extras test)
  "Perform the assertion and record the results."
  (let* ((actual (multiple-value-list (funcall code-thunk)))
         (expected (multiple-value-list (funcall expected-thunk)))
         (result
          (funcall (assert-function type) test expected actual)))
    (if result
        (incf *pass*)
        (push (make-instance
               (assert-class type)
               :form form
               :actual actual
               :expected expected
               :extras (when extras (funcall extras))
               :test test)
              *fail*))
    ;; Return the result
    result))

;;; Unit test results

(defclass test-result ()
  ((name
    :type symbol
    :initarg :name
    :reader name)
   (pass
    :type fixnum
    :initarg :pass
    :reader pass)
   (fail
    :type list
    :initarg :fail
    :reader fail)
   (exerr
    :initarg :exerr
    :reader exerr)
   (run-time
    :initarg :run-time
    :reader run-time
    :documentation
    "Test run time measured in internal time units"))
  (:default-initargs :exerr nil)
  (:documentation
   "Store the results of the unit test."))

(defun print-summary (test-result &optional
                      (stream *standard-output*))
  "Print a summary of the test result."
  (format stream "~&~A: ~S assertions passed, ~S failed"
          (name test-result)
          (pass test-result)
          (length (fail test-result)))
  (if (exerr test-result)
      (format stream ", and an execution error.")
      (write-char #\. stream))
  (terpri stream)
  (terpri stream))

(defun run-code (code)
  "Run the code to test the assertions."
  (funcall (coerce `(lambda () ,@code) 'function)))

(defun run-test-thunk (name code)
  (let ((*pass* 0)
        (*fail* ())
        (start (get-internal-run-time)))
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
                   :run-time (- (get-internal-run-time) start)
                   :exerr condition))))))
      (run-code code))
    ;; Return the result count
    (make-instance
     'test-result
     :name name
     :pass *pass*
     :fail *fail*
     :run-time (- (get-internal-run-time) start))))

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
    (when (plusp (pass result))
      (incf (pass results) (pass result)))
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

(defun summarize-results (results &optional
                          (stream *standard-output*))
  "Print a summary of all results to the stream."
  (let ((pass (pass results))
        (fail (fail results)))
    (format stream "~&Unit Test Summary~%")
    (format stream " | ~D assertions total~%" (+ pass fail))
    (format stream " | ~D passed~%" pass)
    (format stream " | ~D failed~%" fail)
    (format stream " | ~D execution errors~%" (exerr results))
    (format stream " | ~D missing tests~2%"
            (length (missing-tests results)))))

;;; Run the tests

(define-condition test-run-complete ()
  ((results
    :type 'test-results-db
    :initarg :results
    :reader results))
  (:documentation
   "Signaled when a test run is finished."))

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
   (when *signal-results*
     (signal 'test-run-complete :results results))
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
   (when *signal-results*
     (signal 'test-run-complete :results results))
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

(defgeneric print-failures (result &optional stream)
  (:documentation
   "Report the results of the failed assertion."))

(defmethod print-failures :around ((result failure-result) &optional
                                   (stream *standard-output*))
  "Failure header and footer output."
  (format stream "~& | Failed Form: ~S" (form result))
  (call-next-method)
  (when (extras result)
    (format stream "~{~& | ~S => ~S~}~%" (extras result)))
  (format stream "~& |~%"))

(defmethod print-failures ((result failure-result) &optional
                           (stream *standard-output*))
  (format stream "~& | Expected ~{~S~^; ~} " (expected result))
  (format stream "~<~% | ~:;but saw ~{~S~^; ~}~>" (actual result)))

(defmethod print-failures ((result error-result) &optional
                           (stream *standard-output*))
  (format stream "~& | ~@[Should have signalled ~{~S~^; ~} but saw~]"
          (expected result))
  (format stream " ~{~S~^; ~}" (actual result)))

(defmethod print-failures ((result macro-result) &optional
                           (stream *standard-output*))
  (format stream "~& | Should have expanded to ~{~S~^; ~} "
          (expected result))
  (format stream "~<~%~:;but saw ~{~S~^; ~}~>" (actual result)))

(defmethod print-failures ((result output-result) &optional
                           (stream *standard-output*))
  (format stream "~& | Should have printed ~{~S~^; ~} "
          (expected result))
  (format stream "~<~%~:;but saw ~{~S~^; ~}~>"
          (actual result)))

(defmethod print-failures ((result test-result) &optional
                           (stream *standard-output*))
  "Print the failed assertions in the unit test."
  (loop for fail in (fail result) do
        (print-failures fail stream)))

(defmethod print-failures ((results test-results-db) &optional
                           (stream *standard-output*))
  "Print all of the failure tests."
  (loop with db = (database results)
        for test in (failed-tests results)
        as result = (gethash test db)
        do
        (print-failures result stream)
        (print-summary result stream)))

;;; Print errors

(defgeneric print-errors (result &optional stream)
  (:documentation
   "Print the error condition."))

(defmethod print-errors ((result test-result) &optional
                         (stream *standard-output*))
  "Print the error condition."
  (let ((exerr (exerr result))
        (*print-escape* nil))
    (when exerr
      (format stream "~& | Execution error:~% | ~W" exerr)
      (format stream "~& |~%"))))

(defmethod print-errors ((results test-results-db) &optional
                         (stream *standard-output*))
  "Print all of the error tests."
  (loop with db = (database results)
        for test in (error-tests results)
        as result = (gethash test db)
        do
        (print-errors result stream)
        (print-summary result stream)))

;;; Useful equality predicates for tests

(defun logically-equal (x y)
  "Return true if x and y are both false or both true."
  (eql (not x) (not y)))

(defun set-equal (list1 list2 &rest initargs &key key (test #'equal))
  "Return true if every element of list1 is an element of list2 and
vice versa."
  (declare (ignore key test))
  (and
   (listp list1)
   (listp list2)
   (apply #'subsetp list1 list2 initargs)
   (apply #'subsetp list2 list1 initargs)))

(pushnew :lisp-unit common-lisp:*features*)
