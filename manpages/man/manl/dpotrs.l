.TH  DPOTRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DPOTRS - a system of linear equations A*X = B with a symmetric positive definite matrix A using the Cholesky factorization A = U**T*U or A = L*L**T computed by DPOTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE DPOTRS(
UPLO, N, NRHS, A, LDA, B, LDB, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, N, NRHS
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * )
.SH PURPOSE
DPOTRS solves a system of linear equations A*X = B with a symmetric
positive definite matrix A using the Cholesky factorization
A = U**T*U or A = L*L**T computed by DPOTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,N)
The triangular factor U or L from the Cholesky factorization
A = U**T*U or A = L*L**T, as computed by DPOTRF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit, the solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
