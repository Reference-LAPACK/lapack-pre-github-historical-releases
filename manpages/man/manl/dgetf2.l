.TH  DGETF2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DGETF2 - an LU factorization of a general m-by-n matrix A using partial pivoting with row interchanges
.SH SYNOPSIS
.TP 19
SUBROUTINE DGETF2(
M, N, A, LDA, IPIV, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, M, N
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * )
.SH PURPOSE
DGETF2 computes an LU factorization of a general m-by-n matrix A
using partial pivoting with row interchanges.

The factorization has the form
.br
   A = P * L * U
.br
where P is a permutation matrix, L is lower triangular with unit
diagonal elements (lower trapezoidal if m > n), and U is upper
triangular (upper trapezoidal if m < n).
.br

This is the right-looking Level 2 BLAS version of the algorithm.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the m by n matrix to be factored.
On exit, the factors L and U from the factorization
A = P*L*U; the unit diagonal elements of L are not stored.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
IPIV    (output) INTEGER array, dimension (min(M,N))
The pivot indices; for 1 <= i <= min(M,N), row i of the
matrix was interchanged with row IPIV(i).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
.br
> 0: if INFO = k, U(k,k) is exactly zero. The factorization
has been completed, but the factor U is exactly
singular, and division by zero will occur if it is used
to solve a system of equations.
