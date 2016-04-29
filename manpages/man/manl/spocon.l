.TH  SPOCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPOCON - the reciprocal of the condition number (in the 1-norm) of a real symmetric positive definite matrix using the Cholesky factorization A = U**T*U or A = L*L**T computed by SPOTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE SPOCON(
UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,
INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
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
A( LDA, * ), WORK( * )
.SH PURPOSE
SPOCON estimates the reciprocal of the condition number (in the 
1-norm) of a real symmetric positive definite matrix using the
Cholesky factorization A = U**T*U or A = L*L**T computed by SPOTRF.

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
A       (input) REAL array, dimension (LDA,N)
The triangular factor U or L from the Cholesky factorization
A = U**T*U or A = L*L**T, as computed by SPOTRF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
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
