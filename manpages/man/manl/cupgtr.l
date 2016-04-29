.TH  CUPGTR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CUPGTR - a complex unitary matrix Q which is defined as the product of n-1 elementary reflectors H(i) of order n, as returned by CHPTRD using packed storage
.SH SYNOPSIS
.TP 19
SUBROUTINE CUPGTR(
UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDQ, N
.TP 19
.ti +4
COMPLEX
AP( * ), Q( LDQ, * ), TAU( * ), WORK( * )
.SH PURPOSE
CUPGTR generates a complex unitary matrix Q which is defined as the
product of n-1 elementary reflectors H(i) of order n, as returned by
CHPTRD using packed storage:

if UPLO = \(aqU\(aq, Q = H(n-1) . . . H(2) H(1),
.br

if UPLO = \(aqL\(aq, Q = H(1) H(2) . . . H(n-1).
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq: Upper triangular packed storage used in previous
call to CHPTRD;
= \(aqL\(aq: Lower triangular packed storage used in previous
call to CHPTRD.
.TP 8
N       (input) INTEGER
The order of the matrix Q. N >= 0.
.TP 8
AP      (input) COMPLEX array, dimension (N*(N+1)/2)
The vectors which define the elementary reflectors, as
returned by CHPTRD.
.TP 8
TAU     (input) COMPLEX array, dimension (N-1)
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by CHPTRD.
.TP 8
Q       (output) COMPLEX array, dimension (LDQ,N)
The N-by-N unitary matrix Q.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= max(1,N).
.TP 8
WORK    (workspace) COMPLEX array, dimension (N-1)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
