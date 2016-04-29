.TH  ZPBCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZPBCON - the reciprocal of the condition number (in the 1-norm) of a complex Hermitian positive definite band matrix using the Cholesky factorization A = U**H*U or A = L*L**H computed by ZPBTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE ZPBCON(
UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,
RWORK, INFO )
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
DOUBLE
PRECISION ANORM, RCOND
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * )
.TP 19
.ti +4
COMPLEX*16
AB( LDAB, * ), WORK( * )
.SH PURPOSE
ZPBCON estimates the reciprocal of the condition number (in the
1-norm) of a complex Hermitian positive definite band matrix using
the Cholesky factorization A = U**H*U or A = L*L**H computed by
ZPBTRF.

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
or the number of sub-diagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input) COMPLEX*16 array, dimension (LDAB,N)
The triangular factor U or L from the Cholesky factorization
A = U**H*U or A = L*L**H of the band matrix A, stored in the
first KD+1 rows of the array.  The j-th column of U or L is
stored in the j-th column of the array AB as follows:
if UPLO =\(aqU\(aq, AB(kd+1+i-j,j) = U(i,j) for max(1,j-kd)<=i<=j;
if UPLO =\(aqL\(aq, AB(1+i-j,j)    = L(i,j) for j<=i<=min(n,j+kd).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
ANORM   (input) DOUBLE PRECISION
The 1-norm (or infinity-norm) of the Hermitian band matrix A.
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
