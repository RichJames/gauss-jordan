;;;; gauss-jordan.lisp

(in-package #:gauss-jordan)

;; Define a test matrix to use during development
(defparameter *equations* '((4  3 -2  6)
			    (2 -1  3  8)
			    (1  2 -4 10))
  "Test matrix of equations")

(defun get-elem (matrix row col)
  "Returns the value of a specified element in a 2-dimensional matrix."
  (nth col (nth row matrix)))

(defun get-row (matrix row-num)
  "Returns the specified row from a 2-dimensional matrix."
  (nth row-num matrix))

(defun multiply-row (row val)
  "multiplies every element in a list (aka row) by val"
  (mapcar #'(lambda (n) (* val n)) row))

(defun divide-row (row val)
  "Divides every element in a list (aka row) by val"
  (mapcar #'(lambda (n) (/ n val)) row))

(defun diagonal-one (matrix rownum)
  "Converts diagonal value in given row to 1, returns new matrix."
  (loop :for row :in matrix :for i :below (length matrix)
     :if (equal i rownum) :collect (divide-row row (nth i row))
     :else :collect row))

(defun mult-and-add (mod-row mult-row val)
  "Multiply elements in one row by a value, then add those to elements in another row."
  (mapcar #'+ mod-row (multiply-row mult-row val)))

(defun solve-column (matrix col-num)
  "Reduces values in a column to 0, or 1 if the row number = column number."
  (let ((m (diagonal-one matrix col-num)))
    (loop :for row :in m :for i :below (length m)
       :if (equal i col-num) :collect row
       :else :collect (mult-and-add row (get-row m col-num) (- (get-elem m i col-num))))))

(defun matrix-solver (matrix cols)
  "Helper function for solve-matrix, to walk and solve all columns"
  (cond ((equal cols 0) matrix)
	(t (matrix-solver (solve-column matrix (- cols 1)) (- cols 1)))))

(defun solve-matrix (matrix)
  "Uses Gauss-Jordan technique to solve a 2 dimensional matrix"
  (let ((cols (- (length (car matrix)) 1)))
    (matrix-solver matrix cols)))

(defun pretty-print-matrix (matrix)
  (dolist (row matrix)
    (format t "~&~a" row)))


