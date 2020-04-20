# trivial-custom-debugger

## Description

This is a portability library that allows one to fully override the standard debugger provided by their Common Lisp system for situations where binding `*debugger-hook*` is not enough - most notably, for `#'break`.

* The main interface is the `with-debugger` macro that accepts a hook function that would be acceptable for `*debugger-hook*` (it must accept two arguments: a condition that the debugger is invoked with and a similar hook function).
* A functional interface for the above is provided in form of `call-with-debugger`, which accepts a hook function and a zero-argument thunk.
* It is possible, though not advisable, to install a debugger function globally across the whole Lisp system. The function `install-debugger` is provided for that case. (If you want to use it, you might want to contribute a matching `uninstall-debugger` function that restores the Lisp system to the previous state.)

Inside the debugger function:
* the value of `*debugger-hook*` is `nil`,
* the value of the second argument passed to the debugger function is the debugger function itself.

## Purpose

A basic building block that allows one to implement a portable, custom debugger for Common Lisp systems *in place* of the original system-provided one.

## Example

```lisp
TRIVIAL-CUSTOM-DEBUGGER> (with-debugger ((lambda (condition hook)
                                           (declare (ignore hook))
                                           (format t ";; Debugging a ~S!~%" 
                                                   (type-of condition))
                                           (throw :handled t)))
                           (list (catch :handled (error 'error))
                                 (catch :handled (break))
                                 (let ((*break-on-signals* 'error))
                                   (catch :handled (signal 'error)))
                                 (catch :handled 
                                   (invoke-debugger (make-condition 'error)))))
;; Debugging a ERROR!
;; Debugging a SIMPLE-CONDITION!
;; Debugging a SIMPLE-CONDITION!
;; Debugging a ERROR!
(T T T T)
```

## Limitations

Inside `with-debugger` or after calling `install`:
* the value of `*debugger-hook*` is undefined,
* the consequences are undefined if `*debugger-hook*` is set or rebound,
* it is undefined whether the debugger hook or the debugger will be invoked first

## Tests

Very roughly tested on SBCL, CCL, ECL, Clasp, ABCL, CLISP, ACL and LispWorks.

To run the test suite, evaluate `(asdf:test-system :trivial-custom-debugger)`.
