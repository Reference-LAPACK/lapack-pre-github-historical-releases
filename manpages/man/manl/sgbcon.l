.TH  SGBCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SGBCON - the reciprocal of the condition number of a real general band matrix A, in either the 1-norm or the infinity-norm,
.SH SYNOPSIS
.TP 19
SUBROUTINE SGBCON(
NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND,
WORK, IWORK, INFO )
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
REAL
ANORM, RCOND
.TP 19
.ti +4
INTEGER
IPIV( * ), IWORK( * )
.TP 19
.ti +4
REAL
AB( LDAB, * ), WORK( * )
.SH PURPOSE
SGBCON estimates the reciprocal of the condition number of a real
general band matrix A, in either the 1-norm or the infinity-norm,
using the LU factorization computed by SGBTRF.
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
AB      (input) REAL array, dimension (LDAB,N)
Details of the LU factorization of the band matrix A, as
computed by SGBTRF.  U is stored as an upper triangular band
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
ANORM   (input) REAL
If NORM = \(aq1\(aq or \(aqO\(aq, the 1-norm of the original matrix A.
If NORM = \(aqI\(aq, the infinity-norm of the original matrix A.
.TP 8
RCOND   (output) REAL
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(norm(A) * norm(inv(A))).
.TP 8
WORK    (workspace) REAL array, dimension (3*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
