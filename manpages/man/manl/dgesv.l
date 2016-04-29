.TH  DGESV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DGESV - the solution to a real system of linear equations  A * X = B,
.SH SYNOPSIS
.TP 18
SUBROUTINE DGESV(
N, NRHS, A, LDA, IPIV, B, LDB, INFO )
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, N, NRHS
.TP 18
.ti +4
INTEGER
IPIV( * )
.TP 18
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * )
.SH PURPOSE
DGESV computes the solution to a real system of linear equations
   A * X = B,
where A is an N-by-N matrix and X and B are N-by-NRHS matrices.

The LU decomposition with partial pivoting and row interchanges is
used to factor A as
.br
   A = P * L * U,
.br
where P is a permutation matrix, L is unit lower triangular, and U is
upper triangular.  The factored form of A is then used to solve the
system of equations A * X = B.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of linear equations, i.e., the order of the
matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the N-by-N coefficient matrix A.
On exit, the factors L and U from the factorization
A = P*L*U; the unit diagonal elements of L are not stored.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (output) INTEGER array, dimension (N)
The pivot indices that define the permutation matrix P;
row i of the matrix was interchanged with row IPIV(i).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
On entry, the N-by-NRHS matrix of right hand side matrix B.
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
> 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
has been completed, but the factor U is exactly
singular, so the solution could not be computed.
