;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :testing-some-data-structures
  :description "Tests for :some-data-structures."
  :version "0.0"
  :author "arthev"
  :licence "MIT License"
  :depends-on (:prove :some-data-structures)
  :components ((:file "packages")
               (:file "tests")))
