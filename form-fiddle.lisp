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

(defun declaration-p (form)
  (and (listp form) (eql 'declare (first form))))

(defun docstring-p (form)
  (stringp form))

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
    (loop ;; The last expression in a form cannot be a docstring.
          ;; (lambda (foo) "bar") => no docstring, returns "bar".
          for i from 0 below (1- (length body))
          for form in body
          while (declaration-p form)
          finally (when (stringp form)
                    (return form)))))

(defun lambda-declarations (lambda-form)
  "Returns the DECLARATIONS of the lambda-form, if any.

                                                             v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (let ((body (lambda-body lambda-form)))
    (when (stringp (first body))
      (setf body (cdr body)))
    (loop for form in body
          while (or (declaration-p form)
                    (docstring-p form))
          when (declaration-p form)
          collect form)))

(defun lambda-forms (lambda-form)
  "Returns the actual body forms of the lambda-form, if any.

                                                                         v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)"
  (let* ((body (lambda-body lambda-form)))
    (loop for forms on body
          for form = (car body)
          for i from 0 below (1- (length body))
          while (or (declaration-p form)
                    (docstring-p form))
          finally (return
                    (if (declaration-p (car forms))
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
     ;; Now we go on to parsing the body we reached.
     ;; This is annoying because of the interleaving
     ;; of docstrings and declarations.
     ;; F.e. the following is a body with two
     ;; declarations, a docstring and a form:
     ;;
     ;;   (declare) "foo" (declare) "bar"
     ;;
     ;; And this is a body with one declaration, a
     ;; docstring and a form:
     ;;
     ;;   (declare) "foo" "bar"
     ;;
     ;; But this is only a declaration and a form:
     ;;
     ;;   (declare) "foo"
     ;;
     ;; To make matters even more complicated, the
     ;; following has three decls, one docstring, and
     ;; a form. The consequences of multiple docstrings
     ;; like this are undefined as per the hyperspec,
     ;; but it seems allowed so we have to deal.
     ;;
     ;;   (declare) "foo" (declare) "bar" (declare) "baz"
     ;;
     ;; On the other hand, something like this has
     ;; one decl, one docstring and three forms.
     ;;
     ;;   (declare) "foo" "bar" (declare) "baz"
     ;;
     ;; Or at least that's what I'm guessing is the
     ;; intended behaviour suggested by 3.4.11 .
     ;;
     ;; We opt to choose the first occurring docstring.
     (loop with docstring = NIL
           for forms on body
           for form = (car forms)
           for i from 0 below (1- (length body))
           while (or (and (docstring-p form)
                          (or (not docstring)
                              (not (docstring-p (car (cdr forms))))))
                     (declaration-p form))
           if (docstring-p form)
           do (setf docstring (or docstring form))
           else
           collect form into declarations
           finally (return
                     (if (declaration-p (car forms))
                         (list docstring (append declarations (list (car forms))) (cdr forms))
                         (list docstring declarations forms)))))))

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
