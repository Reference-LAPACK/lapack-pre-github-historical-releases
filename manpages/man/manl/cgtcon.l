.TH  CGTCON 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CGTCON - the reciprocal of the condition number of a complex tridiagonal matrix A using the LU factorization as computed by CGTTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CGTCON(
NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,
WORK, INFO )
.TP 19
.ti +4
CHARACTER
NORM
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
D( * ), DL( * ), DU( * ), DU2( * ), WORK( * )
.SH PURPOSE
CGTCON estimates the reciprocal of the condition number of a complex
tridiagonal matrix A using the LU factorization as computed by
CGTTRF.

An estimate is obtained for norm(inv(A)), and the reciprocal of the
condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).

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
DL      (input) COMPLEX array, dimension (N-1)
The (n-1) multipliers that define the matrix L from the
LU factorization of A as computed by CGTTRF.
.TP 8
D       (input) COMPLEX array, dimension (N)
The n diagonal elements of the upper triangular matrix U from
the LU factorization of A.
.TP 8
DU      (input) COMPLEX array, dimension (N-1)
The (n-1) elements of the first superdiagonal of U.
.TP 8
DU2     (input) COMPLEX array, dimension (N-2)
The (n-2) elements of the second superdiagonal of U.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices; for 1 <= i <= n, row i of the matrix was
interchanged with row IPIV(i).  IPIV(i) will always be either
i or i+1; IPIV(i) = i indicates a row interchange was not
required.
.TP 8
ANORM   (input) REAL
If NORM = \(aq1\(aq or \(aqO\(aq, the 1-norm of the original matrix A.
If NORM = \(aqI\(aq, the infinity-norm of the original matrix A.
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
