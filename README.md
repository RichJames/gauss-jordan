# gauss-jordan
### _Rich James_

This is a project to implement the Gauss-Jordan algorithm to solves a set of equations.
It takes a fairly simple approach:

  for each column,\
    divide the row to leave a 1 in the diagonal\
    zero out the remaining column values (except the rightmost column) via row
	  multiplication and addition
	  
Two functions are exported:\
 solve-matrix\
 pretty-print-matrix
 
**Solve-matrix**: takes any 2-dimensional matrix of equations where the coefficients of each of the variables are listed left to right and the constant the equation is equal to is in the rightmost column.  Each equation is in its own row in the matrix.

It then applies the Gauss-Jordan technique to reduce the matrix to its equivalent form where we have 1's along the diagonal from right to left and zeros in all other coefficient positions.  The rightmost column will contain the value of each variable.

**Pretty-print-matrix**: is a simple function to display a 2-dimensional matrix in a more human-readable form. For example, given this matrix: 

 ((2 3 4 10) (1 5 9 12) (-4 7 -8 19))

pretty-print-matrix will display it as:

 (2 3 4 10)\
 (1 5 9 12)\
 (-4 7 -8 10)
 
Obviously, it could be improved to align the columns, but it generally wouldn't be needed as the intent is to pretty print the results of the gauss-jordan result of such a matrix.  E.g., if the above matrix was called *my-matrix*, a call to

(pretty-print-matrix (solve-matrix *my-matrix*))

would result in:

 (1 0 0 3/2)\
 (0 1 0 3)\
 (0 0 1 -1/2)

The result column could be aligned, but this is a good first start.

## License

MIT

