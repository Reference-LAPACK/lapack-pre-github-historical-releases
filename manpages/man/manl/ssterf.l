.TH  SSTERF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SSTERF - all eigenvalues of a symmetric tridiagonal matrix using the Pal-Walker-Kahan variant of the QL or QR algorithm
.SH SYNOPSIS
.TP 19
SUBROUTINE SSTERF(
N, D, E, INFO )
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
REAL
D( * ), E( * )
.SH PURPOSE
SSTERF computes all eigenvalues of a symmetric tridiagonal matrix
using the Pal-Walker-Kahan variant of the QL or QR algorithm.

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the n diagonal elements of the tridiagonal matrix.
On exit, if INFO = 0, the eigenvalues in ascending order.
.TP 8
E       (input/output) REAL array, dimension (N-1)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix.
On exit, E has been destroyed.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  the algorithm failed to find all of the eigenvalues in
a total of 30*N iterations; if INFO = i, then i
elements of E have not converged to zero.
