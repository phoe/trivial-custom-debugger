;;; BSD 2-Clause License
;;;
;;; Copyright (c) 2020, Michał "phoe" Herda
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright notice,
;;; this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;; this list of conditions and the following disclaimer in the documentation
;;; and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY,OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(defpackage #:trivial-custom-debugger/test
  (:use #:cl)
  (:export #:run-tests))

(in-package #:trivial-custom-debugger/test)

(defun run-tests ()
  (let ((failedp nil))
    (macrolet ((test (form)
                 `(loop with name = (car ',form)
                        for (key value) in ,form
                        do (format t "~&~30@S: ~S is ~S~%" name key value)
                        unless value do (setf failedp t))))
      (test (test-error))
      (test (test-invoke-debugger))
      (test (test-break))
      (test (test-signal))
      (test (test-error-hook))
      (test (test-invoke-debugger-hook))
      (test (test-break-hook))
      (test (test-signal-hook))
      (format t "~&~%                   ::: TEST SUITE ~A :::~%"
              (if failedp "FAILED" "PASSED")))))

;;; We verify whether HOOK is equal to the function passed to WITH-DEBUGGER.

;;; We verify whether *DEBUGGER-HOOK* is equal to the function passed to
;;; WITH-DEBUGGER - except for BREAK and SIGNAL, where we verify whether it is
;;; null and that CONTINUE restarts are available.

;;; Except for BREAK and SIGNAL, we verify whether CONDITION is eql to the
;;; condition we have entered the debugger with.

(defmacro null-debugger-hook ()
  '(list :null-debugger-hook (null *debugger-hook*)))

(defmacro t-debugger-hook ()
  '(list :t-debugger-hook (not (null *debugger-hook*))))

(defmacro t-debugger ()
  '(list :t-debugger (not (null hook))))

(defmacro eq-condition ()
  '(list :eq-condition (eq original-condition condition)))

(defmacro typep-condition ()
  '(list :typep-condition (typep condition 'simple-error)))

(defmacro string=-condition-report ()
  '(list :string=-condition-report
    (string= "Test 42" (princ-to-string condition))))

(defmacro continue-restart-found ()
  '(list :continue-restart-found
    (not (not (find-restart 'continue condition)))))

;;; Tests

(defun make-test-condition ()
  (make-condition 'simple-error :format-control "Test ~A"
                                :format-arguments '(42)))

(defun test-error ()
  (let ((original-condition (make-test-condition)))
    (labels ((debugger (condition hook)
               (return-from test-error
                 (list (t-debugger-hook)
                       (t-debugger)
                       (eq-condition)))))
      (trivial-custom-debugger:with-debugger (#'debugger)
        (error original-condition)))))

(defun test-invoke-debugger ()
  (let ((original-condition (make-test-condition)))
    (labels ((debugger (condition hook)
               (return-from test-invoke-debugger
                 (list (t-debugger-hook)
                       (t-debugger)
                       (eq-condition)))))
      (trivial-custom-debugger:with-debugger (#'debugger)
        (let ((*break-on-signals* 'error))
          (invoke-debugger original-condition))))))

(defun test-break ()
  (labels ((debugger (condition hook)
             (return-from test-break
               (list (null-debugger-hook)
                     (t-debugger)
                     (continue-restart-found)
                     (string=-condition-report)))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (break "Test ~A" 42))))

(defun test-signal ()
  (labels ((debugger (condition hook)
             (declare (ignorable condition))
             (return-from test-signal
               (list (null-debugger-hook)
                     (t-debugger)
                     (continue-restart-found)))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (let ((*break-on-signals* 'error))
        (signal (make-test-condition))))))

(defun test-error-hook ()
  (labels ((debugger (condition hook)
             (declare (ignore condition hook))
             (return-from test-error-hook
               '((:hook-before-debugger nil))))
           (hook (condition hook)
             (declare (ignore condition hook))
             (return-from test-error-hook
               '((:hook-before-debugger t)))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (let ((*debugger-hook* #'hook))
        (error (make-test-condition))))))

(defun test-invoke-debugger-hook ()
  (labels ((debugger (condition hook)
             (declare (ignore condition hook))
             (return-from test-invoke-debugger-hook
               '((:hook-before-debugger nil))))
           (hook (condition hook)
             (declare (ignore condition hook))
             (return-from test-invoke-debugger-hook
               '((:hook-before-debugger t)))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (let ((*debugger-hook* #'hook))
        (invoke-debugger (make-test-condition))))))

(defun test-break-hook ()
  (labels ((debugger (condition hook)
             (declare (ignore condition hook))
             (return-from test-break-hook
               '((:debugger-before-hook t))))
           (hook (condition hook)
             (declare (ignore condition hook))
             (return-from test-break-hook
               '((:debugger-before-hook nil)))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (let ((*debugger-hook* #'hook))
        (break "Test ~A" 42)))))

(defun test-signal-hook ()
  (labels ((debugger (condition hook)
             (declare (ignore condition hook))
             (return-from test-signal-hook
               '((:debugger-before-hook t))))
           (hook (condition hook)
             (declare (ignore condition hook))
             (return-from test-signal-hook
               '((:debugger-before-hook nil)))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (let ((*break-on-signals* 'error)
            (*debugger-hook* #'hook))
        (signal (make-test-condition))))))
