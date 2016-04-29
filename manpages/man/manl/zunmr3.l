.TH  ZUNMR3 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZUNMR3 - the general complex m by n matrix C with   Q * C if SIDE = \(aqL\(aq and TRANS = \(aqN\(aq, or   Q\(aq* C if SIDE = \(aqL\(aq and TRANS = \(aqC\(aq, or   C * Q if SIDE = \(aqR\(aq and TRANS = \(aqN\(aq, or   C * Q\(aq if SIDE = \(aqR\(aq and TRANS = \(aqC\(aq,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZUNMR3(
SIDE, TRANS, M, N, K, L, A, LDA, TAU, C, LDC,
WORK, INFO )
.TP 19
.ti +4
CHARACTER
SIDE, TRANS
.TP 19
.ti +4
INTEGER
INFO, K, L, LDA, LDC, M, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
.SH PURPOSE
ZUNMR3 overwrites the general complex m by n matrix C with

where Q is a complex unitary matrix defined as the product of k
elementary reflectors
.br

      Q = H(1) H(2) . . . H(k)
.br

as returned by ZTZRZF. Q is of order m if SIDE = \(aqL\(aq and of order n
if SIDE = \(aqR\(aq.
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= \(aqL\(aq: apply Q or Q\(aq from the Left
.br
= \(aqR\(aq: apply Q or Q\(aq from the Right
.TP 8
TRANS   (input) CHARACTER*1
.br
= \(aqN\(aq: apply Q  (No transpose)
.br
= \(aqC\(aq: apply Q\(aq (Conjugate transpose)
.TP 8
M       (input) INTEGER
The number of rows of the matrix C. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C. N >= 0.
.TP 8
K       (input) INTEGER
The number of elementary reflectors whose product defines
the matrix Q.
If SIDE = \(aqL\(aq, M >= K >= 0;
if SIDE = \(aqR\(aq, N >= K >= 0.
.TP 8
L       (input) INTEGER
The number of columns of the matrix A containing
the meaningful part of the Householder reflectors.
If SIDE = \(aqL\(aq, M >= L >= 0, if SIDE = \(aqR\(aq, N >= L >= 0.
.TP 8
A       (input) COMPLEX*16 array, dimension
(LDA,M) if SIDE = \(aqL\(aq,
(LDA,N) if SIDE = \(aqR\(aq
The i-th row must contain the vector which defines the
elementary reflector H(i), for i = 1,2,...,k, as returned by
ZTZRZF in the last k rows of its array argument A.
A is modified by the routine but restored on exit.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,K).
.TP 8
TAU     (input) COMPLEX*16 array, dimension (K)
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by ZTZRZF.
.TP 8
C       (input/output) COMPLEX*16 array, dimension (LDC,N)
On entry, the m-by-n matrix C.
On exit, C is overwritten by Q*C or Q\(aq*C or C*Q\(aq or C*Q.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension
(N) if SIDE = \(aqL\(aq,
(M) if SIDE = \(aqR\(aq
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
Based on contributions by
.br
  A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA

