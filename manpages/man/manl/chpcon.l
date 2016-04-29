.TH  CHPCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CHPCON - the reciprocal of the condition number of a complex Hermitian packed matrix A using the factorization A = U*D*U**H or A = L*D*L**H computed by CHPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CHPCON(
UPLO, N, AP, IPIV, ANORM, RCOND, WORK, INFO )
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
IPIV( * )
.TP 19
.ti +4
COMPLEX
AP( * ), WORK( * )
.SH PURPOSE
CHPCON estimates the reciprocal of the condition number of a complex
Hermitian packed matrix A using the factorization A = U*D*U**H or
A = L*D*L**H computed by CHPTRF.

An estimate is obtained for norm(inv(A)), and the reciprocal of the
condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the details of the factorization are stored
as an upper or lower triangular matrix.
= \(aqU\(aq:  Upper triangular, form is A = U*D*U**H;
.br
= \(aqL\(aq:  Lower triangular, form is A = L*D*L**H.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
AP      (input) COMPLEX array, dimension (N*(N+1)/2)
The block diagonal matrix D and the multipliers used to
obtain the factor U or L as computed by CHPTRF, stored as a
packed triangular matrix.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D
as determined by CHPTRF.
.TP 8
ANORM   (input) REAL
The 1-norm of the original matrix A.
.TP 8
RCOND   (output) REAL
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
estimate of the 1-norm of inv(A) computed in this routine.
.TP 8
WORK    (workspace) COMPLEX array, dimension (2*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
