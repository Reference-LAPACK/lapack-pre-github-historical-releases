.TH  DSPCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DSPCON - the reciprocal of the condition number (in the 1-norm) of a real symmetric packed matrix A using the factorization A = U*D*U**T or A = L*D*L**T computed by DSPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE DSPCON(
UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK,
INFO )
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
INTEGER
IPIV( * ), IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION AP( * ), WORK( * )
.SH PURPOSE
DSPCON estimates the reciprocal of the condition number (in the
1-norm) of a real symmetric packed matrix A using the factorization
A = U*D*U**T or A = L*D*L**T computed by DSPTRF.

An estimate is obtained for norm(inv(A)), and the reciprocal of the
condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the details of the factorization are stored
as an upper or lower triangular matrix.
= \(aqU\(aq:  Upper triangular, form is A = U*D*U**T;
.br
= \(aqL\(aq:  Lower triangular, form is A = L*D*L**T.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
AP      (input) DOUBLE PRECISION array, dimension (N*(N+1)/2)
The block diagonal matrix D and the multipliers used to
obtain the factor U or L as computed by DSPTRF, stored as a
packed triangular matrix.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D
as determined by DSPTRF.
.TP 8
ANORM   (input) DOUBLE PRECISION
The 1-norm of the original matrix A.
.TP 8
RCOND   (output) DOUBLE PRECISION
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
estimate of the 1-norm of inv(A) computed in this routine.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
.TP 9
IWORK    (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
