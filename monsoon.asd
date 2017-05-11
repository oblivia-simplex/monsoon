(in-package #:asdf-user)

(asdf:defsystem #:monsoon
  :serial t
  :description "Binary rainfall visualizer for data (packets)"
  :depends-on (#:opticl
               #:junk-drawer
               #:plokami)
  :components ((:file "monsoon")
               (:file "packages")))
