.TH  DGTTS2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DGTTS2 - one of the systems of equations  A*X = B or A\(aq*X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE DGTTS2(
ITRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB )
.TP 19
.ti +4
INTEGER
ITRANS, LDB, N, NRHS
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION B( LDB, * ), D( * ), DL( * ), DU( * ), DU2( * )
.SH PURPOSE
DGTTS2 solves one of the systems of equations
   A*X = B  or  A\(aq*X = B,
with a tridiagonal matrix A using the LU factorization computed
by DGTTRF.
.br

.SH ARGUMENTS
.TP 8
ITRANS  (input) INTEGER
Specifies the form of the system of equations.
= 0:  A * X = B  (No transpose)
.br
= 1:  A\(aq* X = B  (Transpose)
.br
= 2:  A\(aq* X = B  (Conjugate transpose = Transpose)
.TP 8
N       (input) INTEGER
The order of the matrix A.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
DL      (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) multipliers that define the matrix L from the
LU factorization of A.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The n diagonal elements of the upper triangular matrix U from
the LU factorization of A.
.TP 8
DU      (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) elements of the first super-diagonal of U.
.TP 8
DU2     (input) DOUBLE PRECISION array, dimension (N-2)
The (n-2) elements of the second super-diagonal of U.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices; for 1 <= i <= n, row i of the matrix was
interchanged with row IPIV(i).  IPIV(i) will always be either
i or i+1; IPIV(i) = i indicates a row interchange was not
required.
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
On entry, the matrix of right hand side vectors B.
On exit, B is overwritten by the solution vectors X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
