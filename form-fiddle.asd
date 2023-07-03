(asdf:defsystem form-fiddle
  :name "Form-Fiddle"
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A collection of utilities to destructure lambda forms."
  :homepage "https://Shinmera.github.io/form-fiddle/"
  :bug-tracker "https://github.com/Shinmera/form-fiddle/issues"
  :source-control (:git "https://github.com/Shinmera/form-fiddle.git")
  :serial T
  :components ((:file "package")
               (:file "form-fiddle")
               (:file "documentation"))
  :depends-on (:documentation-utils))
