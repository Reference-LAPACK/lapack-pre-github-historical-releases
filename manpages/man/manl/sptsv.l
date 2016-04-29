.TH  SPTSV 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPTSV - the solution to a real system of linear equations A*X = B, where A is an N-by-N symmetric positive definite tridiagonal matrix, and X and B are N-by-NRHS matrices
.SH SYNOPSIS
.TP 18
SUBROUTINE SPTSV(
N, NRHS, D, E, B, LDB, INFO )
.TP 18
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 18
.ti +4
REAL
B( LDB, * ), D( * ), E( * )
.SH PURPOSE
SPTSV computes the solution to a real system of linear equations
A*X = B, where A is an N-by-N symmetric positive definite tridiagonal
matrix, and X and B are N-by-NRHS matrices.

A is factored as A = L*D*L**T, and the factored form of A is then
used to solve the system of equations.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the n diagonal elements of the tridiagonal matrix
A.  On exit, the n diagonal elements of the diagonal matrix
D from the factorization A = L*D*L**T.
.TP 8
E       (input/output) REAL array, dimension (N-1)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix A.  On exit, the (n-1) subdiagonal elements of the
unit bidiagonal factor L from the L*D*L**T factorization of
A.  (E can also be regarded as the superdiagonal of the unit
bidiagonal factor U from the U**T*D*U factorization of A.)
.TP 8
B       (input/output) REAL array, dimension (LDB,NRHS)
On entry, the N-by-NRHS right hand side matrix B.
On exit, if INFO = 0, the N-by-NRHS solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the leading minor of order i is not
positive definite, and the solution has not been
computed.  The factorization has not been completed
unless i = N.
