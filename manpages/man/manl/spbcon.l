.TH  SPBCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPBCON - the reciprocal of the condition number (in the 1-norm) of a real symmetric positive definite band matrix using the Cholesky factorization A = U**T*U or A = L*L**T computed by SPBTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE SPBCON(
UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,
IWORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, N
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
AB( LDAB, * ), WORK( * )
.SH PURPOSE
SPBCON estimates the reciprocal of the condition number (in the
1-norm) of a real symmetric positive definite band matrix using the
Cholesky factorization A = U**T*U or A = L*L**T computed by SPBTRF.

An estimate is obtained for norm(inv(A)), and the reciprocal of the
condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangular factor stored in AB;
.br
= \(aqL\(aq:  Lower triangular factor stored in AB.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input) REAL array, dimension (LDAB,N)
The triangular factor U or L from the Cholesky factorization
A = U**T*U or A = L*L**T of the band matrix A, stored in the
first KD+1 rows of the array.  The j-th column of U or L is
stored in the j-th column of the array AB as follows:
if UPLO =\(aqU\(aq, AB(kd+1+i-j,j) = U(i,j) for max(1,j-kd)<=i<=j;
if UPLO =\(aqL\(aq, AB(1+i-j,j)    = L(i,j) for j<=i<=min(n,j+kd).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
ANORM   (input) REAL
The 1-norm (or infinity-norm) of the symmetric band matrix A.
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
