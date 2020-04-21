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

(in-package #:trivial-custom-debugger)

(defun install-debugger (hook)
  "Sets the provided debugger hook function as the system debugger function."
  (assert (functionp hook))
  (macrolet ((named-lambda (name (&rest args) &body body)
                 `(labels ((,name ,args ,@body)) #',name)))
    (flet (#-(or clisp allegro lispworks)
           (make-hook (hook)
             (assert (functionp hook))
             (named-lambda invoke-debugger-hook (condition old-hook)
               (when (and *debugger-hook*
                          (not (eq *debugger-hook* #'invoke-debugger-hook)))
                 (funcall *debugger-hook* condition hook))
               (funcall hook condition old-hook))))
      (let ((hook (make-hook hook)))
        (setf *debugger-hook* hook
              sb-ext:*invoke-debugger-hook* hook)))))

(defun call-with-debugger (hook thunk)
  "Calls the provided thunk function in the dynamic environment where the
provided hook function is set to be the system debugger."
  (let (*debugger-hook* sb-ext:*invoke-debugger-hook*)
    (install-debugger hook)
    (funcall thunk)))

(defmacro with-debugger ((hook) &body body)
  "Executes the provided forms in the dynamic environment where the provided
hook function is set to be the system debugger."
  `(call-with-debugger ,hook (lambda () ,@body)))
