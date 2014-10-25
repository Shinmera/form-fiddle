#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:form-fiddle
  (:use #:cl)
  (:nicknames #:org.shirakumo.form-fiddle)
  (:export
   #:lambda-function
   #:lambda-name
   #:lambda-qualifiers
   #:lambda-lambda-list
   #:lambda-body
   #:lambda-docstring
   #:lambda-declarations
   #:lambda-forms
   #:split-lambda-form
   #:with-destructured-lambda-form))
(in-package #:form-fiddle)

(defun lambda-function (lambda-form)
  "Returns the defining FUNCTION of the lambda-form.
     v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (first lambda-form))

(defun lambda-name (lambda-form)
  "Returns the NAME of the lambda-form, if any.

             v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (unless (listp (second lambda-form))
    (second lambda-form)))

(defun lambda-qualifiers (lambda-form)
  "Returns the QUALIFIERS of the lambda-form.

                      v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (unless (listp (second lambda-form))
    (loop for item in (cddr lambda-form)
          until (listp item)
          collect item)))

(defun lambda-lambda-list (lambda-form)
  "Returns the LAMBDA-LIST of the lambda-form.

                                  v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (loop for item in lambda-form
        when (listp item)
        do (return item)))

(defun lambda-body (lambda-form)
  "Returns all BODY forms of the lambda-form.

                                         |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯v¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (loop for body on lambda-form
        ;; Once we encounter the lambda-list, return the following.
        when (listp (first body))
        do (return (cdr body))))

(defun lambda-docstring (lambda-form)
  "Returns the DOCSTRING of the lambda-form, if any.

                                               v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (let ((body (lambda-body lambda-form)))
    (if (stringp (first body))
        (first body)
        (loop for form in body
              while (and (listp form) (eql 'declare (first form)))
              finally (when (stringp form)
                        (return form))))))

(defun lambda-declarations (lambda-form)
  "Returns the DECLARATIONS of the lambda-form, if any.

                                                             v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (let ((body (lambda-body lambda-form)))
    (when (stringp (first body))
      (setf body (cdr body)))
    (loop for form in body
          while (and (listp form)
                     (eql 'declare (first form)))
          collect form)))

(defun lambda-forms (lambda-form)
  "Returns the actual body forms of the lambda-form, if any.

                                                                         v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (let* ((body (lambda-body lambda-form))
         (doc-first (when (stringp (first body))
                      (setf body (cdr body)) T)))
    (loop for forms on body
          for form = (car forms)
          while (and (listp form)
                     (eql 'declare (first form)))
          finally (return
                    (if (and (not doc-first) (stringp (car forms)))
                        (cdr forms)
                        forms)))))

(defun split-lambda-form (lambda-form)
  "Returns all parts of a lambda-form as a list in the following order:

 FUNCTION NAME QUALIFIERS LAMBDA-LIST DOCSTRING DECLARATIONS FORMS"
  (let ((body lambda-form))
    (append
     (list (first body))
     ;; First is name, rest are qualifiers
     ;; until we reach a list, which must be
     ;; the lambda-list of the form.
     (loop with name = NIL
           for form = (car (setf body (cdr body)))
           until (listp form)
           if name
           collect form into qualifiers
           else
           do (setf name form)
           finally (return
                     (progn
                       (setf body (cdr body))
                       (list name
                             qualifiers
                             form))))
     ;; Now we go on to parsing the body we reached
     ;; here we check if the first is a string, if so,
     ;; set the docstring. Then loop and gather decl
     ;; until we reach something that isn't a declaration.
     ;; If we then have already had a string, just use
     ;; everything else as body. If not, check if it's
     ;; a string and pop that.
     (let* ((docstring (when (stringp (first body))
                         (prog1 (first body)
                           (setf body (cdr body))))))
       (loop for forms on body
             for form = (car forms)
             while (and (listp form)
                        (eql 'declare (first form)))
             collect form into declarations
             finally (return
                       (progn
                         (when (and (not docstring) (stringp (first forms)))
                           (setf docstring (pop forms)))
                         (list docstring
                               declarations
                               forms))))))))

(defmacro with-destructured-lambda-form ((&key function name qualifiers lambda-list docstring declarations forms) expression &body body)
  "Destructures the given EXPRESSION into its lambda-form parts."
  (let ((bindings (list (or function (gensym "FUNCTION"))
                        (or name (gensym "NAME"))
                        (or qualifiers (gensym "QUALIFIERS"))
                        (or lambda-list (gensym "LAMBDA-LIST"))
                        (or docstring (gensym "DOCSTRING"))
                        (or declarations (gensym "DECLARATIONS"))
                        (or forms (gensym "FORMS")))))
    `(destructuring-bind ,bindings
         (split-lambda-form ,expression)
       (declare (ignore ,@(loop for symb in bindings
                                unless (symbol-package symb)
                                collect symb)))
       ,@body)))
