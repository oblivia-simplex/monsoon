(in-package #:asdf-user)

(asdf:defsystem #:monsoon
  :serial t
  :description "Binary rainfall visualizer for data (packets)"
  :depends-on (#:plokami)
  :components ((:file "monsoon")
               (:file "packages")))
