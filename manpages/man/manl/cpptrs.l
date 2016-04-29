.TH  CPPTRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CPPTRS - a system of linear equations A*X = B with a Hermitian positive definite matrix A in packed storage using the Cholesky factorization A = U**H*U or A = L*L**H computed by CPPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CPPTRS(
UPLO, N, NRHS, AP, B, LDB, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 19
.ti +4
COMPLEX
AP( * ), B( LDB, * )
.SH PURPOSE
CPPTRS solves a system of linear equations A*X = B with a Hermitian
positive definite matrix A in packed storage using the Cholesky
factorization A = U**H*U or A = L*L**H computed by CPPTRF.

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
AP      (input) COMPLEX array, dimension (N*(N+1)/2)
The triangular factor U or L from the Cholesky factorization
A = U**H*U or A = L*L**H, packed columnwise in a linear
array.  The j-th column of U or L is stored in the array AP
as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = U(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = L(i,j) for j<=i<=n.
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
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
