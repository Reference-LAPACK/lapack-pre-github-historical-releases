.TH  DPTTRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DPTTRS - a tridiagonal system of the form  A * X = B using the L*D*L\(aq factorization of A computed by DPTTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE DPTTRS(
N, NRHS, D, E, B, LDB, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 19
.ti +4
DOUBLE
PRECISION B( LDB, * ), D( * ), E( * )
.SH PURPOSE
DPTTRS solves a tridiagonal system of the form
   A * X = B
using the L*D*L\(aq factorization of A computed by DPTTRF.  D is a
diagonal matrix specified in the vector D, L is a unit bidiagonal
matrix whose subdiagonal is specified in the vector E, and X and B
are N by NRHS matrices.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the tridiagonal matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The n diagonal elements of the diagonal matrix D from the
L*D*L\(aq factorization of A.
.TP 8
E       (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) subdiagonal elements of the unit bidiagonal factor
L from the L*D*L\(aq factorization of A.  E can also be regarded
as the superdiagonal of the unit bidiagonal factor U from the
factorization A = U\(aq*D*U.
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
On entry, the right hand side vectors B for the system of
linear equations.
On exit, the solution vectors, X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
