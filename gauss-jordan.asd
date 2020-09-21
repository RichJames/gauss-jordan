;;;; gauss-jordan.asd

(asdf:defsystem #:gauss-jordan
  :description "Implentation of the  gauss-jordan algorithm to solve sets of equations"
  :author "Rich James"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "gauss-jordan")))
