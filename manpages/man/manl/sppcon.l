.TH  SPPCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPPCON - the reciprocal of the condition number (in the 1-norm) of a real symmetric positive definite packed matrix using the Cholesky factorization A = U**T*U or A = L*L**T computed by SPPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE SPPCON(
UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )
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
REAL
ANORM, RCOND
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
AP( * ), WORK( * )
.SH PURPOSE
SPPCON estimates the reciprocal of the condition number (in the
1-norm) of a real symmetric positive definite packed matrix using
the Cholesky factorization A = U**T*U or A = L*L**T computed by
SPPTRF.

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
AP      (input) REAL array, dimension (N*(N+1)/2)
The triangular factor U or L from the Cholesky factorization
A = U**T*U or A = L*L**T, packed columnwise in a linear
array.  The j-th column of U or L is stored in the array AP
as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = U(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = L(i,j) for j<=i<=n.
.TP 8
ANORM   (input) REAL
The 1-norm (or infinity-norm) of the symmetric matrix A.
.TP 8
RCOND   (output) REAL
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
estimate of the 1-norm of inv(A) computed in this routine.
.TP 8
WORK    (workspace) REAL array, dimension (3*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
