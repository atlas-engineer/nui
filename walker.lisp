;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:nui)

;; Utilities for code walking and, consequently, Lisp syntax highlighting.

(macrolet ((def (name doc)
             `(defvar ,name nil ,doc)))
  (def *functions* "The list of functions parsed by `resolve' and `resolve-form'.")
  (def *variables* "The list of variables parsed by `resolve' and `resolve-form'.")
  (def *macros* "The list of macros parsed by `resolve' and `resolve-form'.")
  (def *types* "The list of types parsed by `resolve' and `resolve-form'."))

(defvar *specials*
  '(quote
    flet labels symbol-macrolet macrolet
    block catch eval-when progv lambda
    progn prog1 unwind-protect tagbody setf setq multiple-value-prog1
    let let* prog prog*
    return-from throw the
    multiple-value-call funcall apply
    function
    go locally)
  "CL special forms.")

(defgeneric resolve (object)
  (:method ((object t))
    (declare (ignore object)))
  (:method ((object symbol))
    (push object *variables*))
  (:method ((object list))
    (resolve-form (first object) (rest object)))
  (:documentation "Resolves OBJECT for code highlighting.
Calls `resolve-form' for lists.
Symbols most likely end up in one of `*variables*', `*functions*',
`*macros*', or `*types*', unless methods are overloaded."))

(defgeneric resolve-form (head args)
  (:documentation "Resolve (head . ARGS) form.
Specializations should push symbols to either `*variables*',
`*functions*', `*macros*', or `*types*'."))

(defmethod resolve-form ((head (eql 'make-instance)) (args list))
  (pushnew head *functions*)
  (let ((class (first args)))
    (cond
      ((and (listp class)
            (eq 'quote (first class)))
       (push (second class) *types*))
      ((listp class)
       (resolve class))
      (t (resolve class))))
  (loop for (arg) on (cddr args) by #'cddr
        do (resolve arg)))

(defmethod resolve-form ((head (eql 'defclass)) (args list))
  (pushnew head *macros*)
  (pushnew (first args) *types*))

(defmethod resolve-form ((head (eql 'defgeneric)) (args list))
  (pushnew (first args) *types*)
  (pushnew head *macros*)
  (loop for form in args
        ;; More `defgeneric' options?
        when (eq :method (first form))
          do (mapc #'resolve (cddr form))))

(macrolet ((resolve-forms ((&rest heads) (&rest args) &body body)
             "Create multiple `resolve-form' methods.
For each HEADS, a method is created with ((head HEAD) (args list)) arglist.
ARGS are destructuring list applied to args of the form."
             `(progn ,@(loop for head in heads
                             collect `(defmethod resolve-form ((head (eql (quote ,head))) (args list))
                                        (declare (ignorable head))
                                        (destructuring-bind (,@args)
                                            args
                                          ,@body))))))
  (resolve-forms
   (flet labels symbol-macrolet macrolet)
   ((&rest bindings) &body body)
   (mapc (lambda (b) (mapc #'resolve (cddr b)))
         bindings)
   (mapc #'resolve body))

  (resolve-forms
   (block catch eval-when progv lambda)
   (arg &body body)
   (declare (ignore arg))
   (mapc #'resolve body))

  (resolve-forms
   (let let* prog prog*)
   ((&rest bindings) &body body)
   (mapcar (alexandria:compose #'resolve #'second #'uiop:ensure-list)
           bindings)
   (mapc #'resolve body))

  (resolve-forms
   (multiple-value-call funcall apply)
   (function &rest args)
   (when (and (listp function)
              (member (first function) '(quote function)))
     (pushnew (second function) *functions*))
   (mapc #'resolve args))

  (resolve-forms
   (typecase)
   (form &rest cases)
   (resolve form)
   (loop for (types . forms) in cases
         unless (member types '(t otherwise))
           do (loop for type in (uiop:ensure-list types)
                    do (pushnew type *types*))
         do (mapc #'resolve forms)))

  (resolve-forms
   (case)
   (form &rest cases)
   (resolve form)
   (dolist (forms (mapcar #'rest cases))
     (mapc #'resolve forms)))

  (resolve-forms
   (function)
   (value)
   (pushnew value *functions*)))

(defmethod resolve-form ((head list) (args list))
  (resolve head)
  (mapc #'resolve args))

(defmethod resolve-form ((head symbol) (args list))
  (cond
    ((macro-function head)
     (pushnew head *macros*)
     ;; This is terribly naive---we simply search for first list-like
     ;; form in macro args, hoping it's the body. Likely to cause of
     ;; false-positives, but the check for `fboundp' below should
     ;; filter out some of them.
     (mapc #'resolve (member-if #'listp args)))
    ((fboundp head)
     (pushnew head *functions*)
     (mapc #'resolve args))))
