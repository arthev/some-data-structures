;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :some-data-structures
  :description "A collection of some data structures."
  :version "0.0"
  :author "arthev"
  :licence "MIT License"
  :components ((:file "packages")
               (:file "generics")
               (:file "pairing-heap")
               (:file "binary-heap")
               (:file "priority-queue")))
