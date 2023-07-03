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
   #:with-destructured-lambda-form
   #:split-body-options
   #:with-body-options))
