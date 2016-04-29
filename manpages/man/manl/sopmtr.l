.TH  SOPMTR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SOPMTR - the general real M-by-N matrix C with   SIDE = \(aqL\(aq SIDE = \(aqR\(aq TRANS = \(aqN\(aq
.SH SYNOPSIS
.TP 19
SUBROUTINE SOPMTR(
SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,
INFO )
.TP 19
.ti +4
CHARACTER
SIDE, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDC, M, N
.TP 19
.ti +4
REAL
AP( * ), C( LDC, * ), TAU( * ), WORK( * )
.SH PURPOSE
SOPMTR overwrites the general real M-by-N matrix C with
TRANS = \(aqT\(aq:      Q**T * C       C * Q**T
.br

where Q is a real orthogonal matrix of order nq, with nq = m if
SIDE = \(aqL\(aq and nq = n if SIDE = \(aqR\(aq. Q is defined as the product of
nq-1 elementary reflectors, as returned by SSPTRD using packed
storage:
.br

if UPLO = \(aqU\(aq, Q = H(nq-1) . . . H(2) H(1);
.br

if UPLO = \(aqL\(aq, Q = H(1) H(2) . . . H(nq-1).
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= \(aqL\(aq: apply Q or Q**T from the Left;
.br
= \(aqR\(aq: apply Q or Q**T from the Right.
.TP 8
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq: Upper triangular packed storage used in previous
call to SSPTRD;
= \(aqL\(aq: Lower triangular packed storage used in previous
call to SSPTRD.
.TP 8
TRANS   (input) CHARACTER*1
= \(aqN\(aq:  No transpose, apply Q;
.br
= \(aqT\(aq:  Transpose, apply Q**T.
.TP 8
M       (input) INTEGER
The number of rows of the matrix C. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C. N >= 0.
.TP 8
AP      (input) REAL array, dimension
(M*(M+1)/2) if SIDE = \(aqL\(aq
(N*(N+1)/2) if SIDE = \(aqR\(aq
The vectors which define the elementary reflectors, as
returned by SSPTRD.  AP is modified by the routine but
restored on exit.
.TP 8
TAU     (input) REAL array, dimension (M-1) if SIDE = \(aqL\(aq
or (N-1) if SIDE = \(aqR\(aq
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by SSPTRD.
.TP 8
C       (input/output) REAL array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace) REAL array, dimension
(N) if SIDE = \(aqL\(aq
(M) if SIDE = \(aqR\(aq
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
