;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
#|

  Test Anything Protocol (TAP) support for LISP-UNIT

  Copyright (c) 2009-2016, Ryan Davis <ryan@acceleration.net>

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
  [TAP]: http://testanything.org/wiki/index.php/Main_Page

|#

(in-package :lisp-unit)

;;; Symbols exported from the TAP extension

(export '(write-tap write-tap-to-file))

(defun run-time-s (test-result)
  "calculate the run-time of the test in seconds"
  (/ (run-time test-result)
     internal-time-units-per-second))

(defun %write-tap-test-result (name test-result i stream)
  "Output a single test, taking care to ensure the indentation level
is the same before and after invocation."
  (pprint-logical-block (stream nil)
    (format stream
            "~:[ok~;not ok~] ~d ~s (~,2f s)"
            (or (fail test-result)
                (exerr test-result))
            i name
            (run-time-s test-result))
    (when (or (fail test-result)
              (exerr test-result))
      ;; indent only takes affect after a newline, so force one
      (format stream "~2I~:@_---~@:_")
      (when (exerr test-result)
        (format stream "message: |~4I~_~s~2I~@:_" (exerr test-result)))
      (when (fail test-result)
        (format stream "message: ~d failed assertions~@:_"
                (length (fail test-result))))
      (format stream "..."))
    ;; always reset to zero and force a newline
    (format stream "~0I~@:_")))

(defun write-tap (test-results &optional (stream *standard-output*))
  "Write the test results to `stream` in TAP format. Returns the test
results."
  (check-type test-results test-results-db)
  (let ((i 0)
        (*print-pretty* T))
    (format stream "TAP version 13~%1..~d~%"
            (hash-table-count (database test-results)))
    (maphash
     #'(lambda (name test-result)
         (%write-tap-test-result name test-result (incf i) stream))
     (database test-results)))
  test-results)

(defun write-tap-to-file (test-results path)
  "write the test results to `path` in TAP format, overwriting `path`.
Returns pathname to the output file"
  (check-type path (or string pathname))
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-tap test-results s))
  (truename path))
