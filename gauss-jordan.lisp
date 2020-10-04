;;;; gauss-jordan.lisp

(in-package #:gauss-jordan)

;; Define test matrices to use during development
(defparameter *m1* '((1  2 -4 10)
		     (4  3 -2  6)
		     (2 -1  3  8)))

(defparameter *m2* '((0 10 10 10 20)
		     (1  1 10 10 20)
		     (2  5  1 10 20)
		     (3  2  5  0 20)))

(defparameter *m3* '((1 10 10 10 20)
		     (5 50 10 10 20)
		     (2  5  1 10 20)
		     (3  2  5  0 20)))

(defparameter *m4* '((1 10 10 10 20)
		     (5 50 10 10 20)
		     (0  0  0  0 20)
		     (3  2  5  0 20)))

(defparameter *m5* '((1 10 10 10 20)
		     (5 50 10 10 20)
		     (0  0  0  0  0)
		     (3  2  5  0 20)))


(defun solve-matrix (matrix)
  "Reduce a matrix using Gauss Jordan Elimination."
  (do ((m (move-zero-rows-down matrix))
       (num-rows (length matrix))
       (curr-row 0 (1+ curr-row)))
      ((= curr-row num-rows) m)
    (progn
      (if (= 0 (get-elem m curr-row curr-row)) (setf m (move-largest-row-up m curr-row)))
      (unless (= 0 (get-elem m curr-row curr-row))
	(setf m (make-leading-entry-1 m curr-row))
	(setf m (solve-column m curr-row))))))

;; Moves all rows that contain only 0's to the bottom of the matrix.
(defun move-zero-rows-down (matrix)
  (loop :for row :in matrix
     :if (zero-row-p row) :collect row :into zero-rows
     :else :collect row :into non-zero-rows
     :finally (return (append non-zero-rows zero-rows))))

(defun zero-row-p (row)
  (every #'zerop row))

(defun get-elem (matrix row col)
  "Returns the value of a specified element in a 2-dimensional matrix."
  (nth col (nth row matrix)))

(defun make-leading-entry-1 (matrix pos)
  (loop :for i :below (length matrix)
     :for row :in matrix
     :if (= i pos) :collect (divide-row row (nth pos row))
     :else :collect row))

(defun divide-row (row val)
  "Divides every element in a list (aka row) by val"
  (cond ((= val 0) row)
	(t (mapcar #'(lambda (n) (/ n val)) row))))

;; This moves the row with the largest value in the specified column
;; up in the matrix.  However, the col value restricts what rows are
;; looked at and how far up in the matrix a row will move.  For a given
;; col value, only rows starting at row = col and lower are looked at.
;; And when a row is moved up, it only moves up as high as row = col.
(defun move-largest-row-up (matrix col)
  (let ((max-row (find-row-with-largest-val matrix col)))
    (cond ((not max-row) matrix)
	  (t (loop :for i :below (length matrix)
		:for row :in matrix
		:if (< i col) :collect row :into first-rows
		:else :if (= i max-row) :collect row :into move-row
		:else :collect row :into remaining-rows
		:finally (return (append first-rows move-row remaining-rows)))))))

(defun find-row-with-largest-val (matrix col)
  "Finds the row with the largest non-zero value in the specified column."
  (let ((col-vals (get-col-vals matrix col)))
    (if (zero-row-p col-vals)
	col
	(+ col (find-pos-with-largest col-vals)))))

;; Gets the values in the specified column in the matrix, but only gets those
;; values starting in row = col and lower.
(defun get-col-vals (matrix col)
  (mapcar #'(lambda (n) (nth col n)) (nthcdr col matrix)))

(defun find-pos-with-largest (nums)
  "Find position of largest value in the list."
  (if (zero-row-p nums)
      0
      (let ((maxval (apply 'max (remove-if #'zerop nums))))
	(position maxval nums))))

(defun solve-column (matrix col-num)
  "Reduces values in a column to 0 for all rows where row not = col."
  (loop :for row :in matrix :for i :below (length matrix)
     :if (equal i col-num) :collect row
     :else :collect (mult-and-add row (get-row matrix col-num) (- (get-elem matrix i col-num)))))

(defun mult-and-add (mod-row mult-row val)
  "Multiply elements in one row by a value, then add those to elements in another row."
  (mapcar #'+ mod-row (multiply-row mult-row val)))

(defun multiply-row (row val)
  "multiplies every element in a list (aka row) by val"
  (mapcar #'(lambda (n) (* val n)) row))

(defun get-row (matrix row-num)
  "Returns the specified row from a 2-dimensional matrix."
  (nth row-num matrix))

(defun pretty-print-matrix (matrix)
  (dolist (row matrix)
    (format t "~&~a" row)))


;; Utility predicates that are not currently being used.  These could be used to determine whether or not
;; a set of equations could be solved.  For now, assessing the result is being left to the caller.

(defun zeros-and-val-p (row)
  "Returns T if all values but rightmost one are 0."
  (let ((r (cdr (reverse row))))
    (and (not (zero-row-p row)) (zero-row-p r))))

(defun infinite-solutions-p (matrix)
  (find-if #'zero-row-p matrix))

(defun no-solutions-p (matrix)
  (find-if #'zeros-and-val-p matrix))

