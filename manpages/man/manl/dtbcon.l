.TH  DTBCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DTBCON - the reciprocal of the condition number of a triangular band matrix A, in either the 1-norm or the infinity-norm
.SH SYNOPSIS
.TP 19
SUBROUTINE DTBCON(
NORM, UPLO, DIAG, N, KD, AB, LDAB, RCOND, WORK,
IWORK, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, NORM, UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, N
.TP 19
.ti +4
DOUBLE
PRECISION RCOND
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION AB( LDAB, * ), WORK( * )
.SH PURPOSE
DTBCON estimates the reciprocal of the condition number of a
triangular band matrix A, in either the 1-norm or the infinity-norm.

The norm of A is computed and an estimate is obtained for
norm(inv(A)), then the reciprocal of the condition number is
computed as
.br
   RCOND = 1 / ( norm(A) * norm(inv(A)) ).
.br

.SH ARGUMENTS
.TP 8
NORM    (input) CHARACTER*1
Specifies whether the 1-norm condition number or the
infinity-norm condition number is required:
.br
= \(aq1\(aq or \(aqO\(aq:  1-norm;
.br
= \(aqI\(aq:         Infinity-norm.
.TP 8
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq:  A is upper triangular;
.br
= \(aqL\(aq:  A is lower triangular.
.TP 8
DIAG    (input) CHARACTER*1
.br
= \(aqN\(aq:  A is non-unit triangular;
.br
= \(aqU\(aq:  A is unit triangular.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of superdiagonals or subdiagonals of the
triangular band matrix A.  KD >= 0.
.TP 8
AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
The upper or lower triangular band matrix A, stored in the
first kd+1 rows of the array. The j-th column of A is stored
in the j-th column of the array AB as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
If DIAG = \(aqU\(aq, the diagonal elements of A are not referenced
and are assumed to be 1.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
RCOND   (output) DOUBLE PRECISION
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(norm(A) * norm(inv(A))).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
