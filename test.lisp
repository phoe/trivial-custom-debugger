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
  (assert (test-error))
  (assert (test-break))
  (assert (test-signal))
  (assert (test-invoke-debugger))
  t)

(define-condition my-error (error) ())

(defun test-error ()
  (labels ((debugger (condition hook)
             (declare (ignore hook))
             (when (and (null *debugger-hook*)
                        (typep condition 'my-error))
               (return-from test-error t))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (error 'my-error))))

(defun test-break ()
  (labels ((debugger (condition hook)
             (declare (ignore hook))
             (when (and (null *debugger-hook*)
                        (find-restart 'continue condition)
                        (typep condition 'simple-condition)
                        (string= "Test 42" (princ-to-string condition)))
               (return-from test-break t))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (break "Test ~A" 42))))

(defun test-signal ()
  (labels ((debugger (condition hook)
             (declare (ignore hook))
             (when (and (null *debugger-hook*)
                        (find-restart 'continue condition))
               (return-from test-signal t))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (let ((*break-on-signals* 'error))
        (signal 'my-error)))))

(defun test-invoke-debugger ()
  (labels ((debugger (condition hook)
             (declare (ignore hook))
             (when (and (null *debugger-hook*)
                        (typep condition 'my-error))
               (return-from test-invoke-debugger t))))
    (trivial-custom-debugger:with-debugger (#'debugger)
      (let ((*break-on-signals* 'error))
        (invoke-debugger (make-condition 'my-error))))))
