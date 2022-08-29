;;;; virtual-machine.asd

(asdf:defsystem #:virtual-machine
  :description "A JVM-like virtual machine for the Hack platform."
  :author "Gin Cui"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:split-sequence)
  :components ((:file "package")
               (:file "virtual-machine")))
