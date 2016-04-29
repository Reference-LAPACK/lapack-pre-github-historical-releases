.TH  CPOCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CPOCON - the reciprocal of the condition number (in the 1-norm) of a complex Hermitian positive definite matrix using the Cholesky factorization A = U**H*U or A = L*L**H computed by CPOTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CPOCON(
UPLO, N, A, LDA, ANORM, RCOND, WORK, RWORK,
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
REAL
RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), WORK( * )
.SH PURPOSE
CPOCON estimates the reciprocal of the condition number (in the
1-norm) of a complex Hermitian positive definite matrix using the
Cholesky factorization A = U**H*U or A = L*L**H computed by CPOTRF.

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
A       (input) COMPLEX array, dimension (LDA,N)
The triangular factor U or L from the Cholesky factorization
A = U**H*U or A = L*L**H, as computed by CPOTRF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
ANORM   (input) REAL
The 1-norm (or infinity-norm) of the Hermitian matrix A.
.TP 8
RCOND   (output) REAL
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
estimate of the 1-norm of inv(A) computed in this routine.
.TP 8
WORK    (workspace) COMPLEX array, dimension (2*N)
.TP 8
RWORK   (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
