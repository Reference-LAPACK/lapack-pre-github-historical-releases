.TH  ZGTTRF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZGTTRF - an LU factorization of a complex tridiagonal matrix A using elimination with partial pivoting and row interchanges
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGTTRF(
N, DL, D, DU, DU2, IPIV, INFO )
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
COMPLEX*16
D( * ), DL( * ), DU( * ), DU2( * )
.SH PURPOSE
ZGTTRF computes an LU factorization of a complex tridiagonal matrix A
using elimination with partial pivoting and row interchanges.

The factorization has the form
.br
   A = L * U
.br
where L is a product of permutation and unit lower bidiagonal
matrices and U is upper triangular with nonzeros in only the main
diagonal and first two superdiagonals.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.
.TP 8
DL      (input/output) COMPLEX*16 array, dimension (N-1)
On entry, DL must contain the (n-1) sub-diagonal elements of
A.

On exit, DL is overwritten by the (n-1) multipliers that
define the matrix L from the LU factorization of A.
.TP 8
D       (input/output) COMPLEX*16 array, dimension (N)
On entry, D must contain the diagonal elements of A.

On exit, D is overwritten by the n diagonal elements of the
upper triangular matrix U from the LU factorization of A.
.TP 8
DU      (input/output) COMPLEX*16 array, dimension (N-1)
On entry, DU must contain the (n-1) super-diagonal elements
of A.

On exit, DU is overwritten by the (n-1) elements of the first
super-diagonal of U.
.TP 8
DU2     (output) COMPLEX*16 array, dimension (N-2)
On exit, DU2 is overwritten by the (n-2) elements of the
second super-diagonal of U.
.TP 8
IPIV    (output) INTEGER array, dimension (N)
The pivot indices; for 1 <= i <= n, row i of the matrix was
interchanged with row IPIV(i).  IPIV(i) will always be either
i or i+1; IPIV(i) = i indicates a row interchange was not
required.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -k, the k-th argument had an illegal value
.br
> 0:  if INFO = k, U(k,k) is exactly zero. The factorization
has been completed, but the factor U is exactly
singular, and division by zero will occur if it is used
to solve a system of equations.
