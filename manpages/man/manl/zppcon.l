.TH  ZPPCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZPPCON - the reciprocal of the condition number (in the 1-norm) of a complex Hermitian positive definite packed matrix using the Cholesky factorization A = U**H*U or A = L*L**H computed by ZPPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE ZPPCON(
UPLO, N, AP, ANORM, RCOND, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
DOUBLE
PRECISION ANORM, RCOND
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * )
.TP 19
.ti +4
COMPLEX*16
AP( * ), WORK( * )
.SH PURPOSE
ZPPCON estimates the reciprocal of the condition number (in the
1-norm) of a complex Hermitian positive definite packed matrix using
the Cholesky factorization A = U**H*U or A = L*L**H computed by
ZPPTRF.

An estimate is obtained for norm(inv(A)), and the reciprocal of the
condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).

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
AP      (input) COMPLEX*16 array, dimension (N*(N+1)/2)
The triangular factor U or L from the Cholesky factorization
A = U**H*U or A = L*L**H, packed columnwise in a linear
array.  The j-th column of U or L is stored in the array AP
as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = U(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = L(i,j) for j<=i<=n.
.TP 8
ANORM   (input) DOUBLE PRECISION
The 1-norm (or infinity-norm) of the Hermitian matrix A.
.TP 8
RCOND   (output) DOUBLE PRECISION
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
estimate of the 1-norm of inv(A) computed in this routine.
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (2*N)
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
