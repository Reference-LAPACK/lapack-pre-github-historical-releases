.TH  SGECON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SGECON - the reciprocal of the condition number of a general real matrix A, in either the 1-norm or the infinity-norm, using the LU factorization computed by SGETRF
.SH SYNOPSIS
.TP 19
SUBROUTINE SGECON(
NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,
INFO )
.TP 19
.ti +4
CHARACTER
NORM
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
SGECON estimates the reciprocal of the condition number of a general
real matrix A, in either the 1-norm or the infinity-norm, using
the LU factorization computed by SGETRF.

An estimate is obtained for norm(inv(A)), and the reciprocal of the
condition number is computed as
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
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input) REAL array, dimension (LDA,N)
The factors L and U from the factorization A = P*L*U
as computed by SGETRF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
ANORM   (input) REAL
If NORM = \(aq1\(aq or \(aqO\(aq, the 1-norm of the original matrix A.
If NORM = \(aqI\(aq, the infinity-norm of the original matrix A.
.TP 8
RCOND   (output) REAL
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(norm(A) * norm(inv(A))).
.TP 8
WORK    (workspace) REAL array, dimension (4*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
