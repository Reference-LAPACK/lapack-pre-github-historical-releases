.TH  ZGBCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZGBCON - the reciprocal of the condition number of a complex general band matrix A, in either the 1-norm or the infinity-norm,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGBCON(
NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND,
WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
NORM
.TP 19
.ti +4
INTEGER
INFO, KL, KU, LDAB, N
.TP 19
.ti +4
DOUBLE
PRECISION ANORM, RCOND
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * )
.TP 19
.ti +4
COMPLEX*16
AB( LDAB, * ), WORK( * )
.SH PURPOSE
ZGBCON estimates the reciprocal of the condition number of a complex
general band matrix A, in either the 1-norm or the infinity-norm,
using the LU factorization computed by ZGBTRF.
.br

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
KL      (input) INTEGER
The number of subdiagonals within the band of A.  KL >= 0.
.TP 8
KU      (input) INTEGER
The number of superdiagonals within the band of A.  KU >= 0.
.TP 8
AB      (input) COMPLEX*16 array, dimension (LDAB,N)
Details of the LU factorization of the band matrix A, as
computed by ZGBTRF.  U is stored as an upper triangular band
matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
the multipliers used during the factorization are stored in
rows KL+KU+2 to 2*KL+KU+1.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices; for 1 <= i <= N, row i of the matrix was
interchanged with row IPIV(i).
.TP 8
ANORM   (input) DOUBLE PRECISION
If NORM = \(aq1\(aq or \(aqO\(aq, the 1-norm of the original matrix A.
If NORM = \(aqI\(aq, the infinity-norm of the original matrix A.
.TP 8
RCOND   (output) DOUBLE PRECISION
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(norm(A) * norm(inv(A))).
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (2*N)
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
