#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem form-fiddle
  :name "Form-Fiddle"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of utilities to destructure lambda forms."
  :homepage "https://github.com/Shinmera/form-fiddle"
  :serial T
  :components ((:file "form-fiddle"))
  :depends-on ())
