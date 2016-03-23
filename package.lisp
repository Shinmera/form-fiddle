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
   #:with-destructured-lambda-form
   #:split-body-options
   #:with-body-options))
