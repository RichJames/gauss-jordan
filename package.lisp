;;;; package.lisp

(defpackage #:gauss-jordan
  (:use #:cl)
  (:export :solve-matrix
	   :pretty-print-matrix
	   :infinite-solutions-p
	   :no-solutions-p))
