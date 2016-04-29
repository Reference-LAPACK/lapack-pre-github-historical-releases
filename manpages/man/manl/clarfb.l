.TH  CLARFB 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLARFB - a complex block reflector H or its transpose H\(aq to a complex M-by-N matrix C, from either the left or the right
.SH SYNOPSIS
.TP 19
SUBROUTINE CLARFB(
SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
T, LDT, C, LDC, WORK, LDWORK )
.TP 19
.ti +4
CHARACTER
DIRECT, SIDE, STOREV, TRANS
.TP 19
.ti +4
INTEGER
K, LDC, LDT, LDV, LDWORK, M, N
.TP 19
.ti +4
COMPLEX
C( LDC, * ), T( LDT, * ), V( LDV, * ),
WORK( LDWORK, * )
.SH PURPOSE
CLARFB applies a complex block reflector H or its transpose H\(aq to a
complex M-by-N matrix C, from either the left or the right.

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= \(aqL\(aq: apply H or H\(aq from the Left
.br
= \(aqR\(aq: apply H or H\(aq from the Right
.TP 8
TRANS   (input) CHARACTER*1
.br
= \(aqN\(aq: apply H (No transpose)
.br
= \(aqC\(aq: apply H\(aq (Conjugate transpose)
.TP 8
DIRECT  (input) CHARACTER*1
Indicates how H is formed from a product of elementary
reflectors
= \(aqF\(aq: H = H(1) H(2) . . . H(k) (Forward)
.br
= \(aqB\(aq: H = H(k) . . . H(2) H(1) (Backward)
.TP 8
STOREV  (input) CHARACTER*1
Indicates how the vectors which define the elementary
reflectors are stored:
.br
= \(aqC\(aq: Columnwise
.br
= \(aqR\(aq: Rowwise
.TP 8
M       (input) INTEGER
The number of rows of the matrix C.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C.
.TP 8
K       (input) INTEGER
The order of the matrix T (= the number of elementary
reflectors whose product defines the block reflector).
.TP 8
V       (input) COMPLEX array, dimension
(LDV,K) if STOREV = \(aqC\(aq
(LDV,M) if STOREV = \(aqR\(aq and SIDE = \(aqL\(aq
(LDV,N) if STOREV = \(aqR\(aq and SIDE = \(aqR\(aq
The matrix V. See further details.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V.
If STOREV = \(aqC\(aq and SIDE = \(aqL\(aq, LDV >= max(1,M);
if STOREV = \(aqC\(aq and SIDE = \(aqR\(aq, LDV >= max(1,N);
if STOREV = \(aqR\(aq, LDV >= K.
.TP 8
T       (input) COMPLEX array, dimension (LDT,K)
The triangular K-by-K matrix T in the representation of the
block reflector.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= K.
.TP 8
C       (input/output) COMPLEX array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by H*C or H\(aq*C or C*H or C*H\(aq.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace) COMPLEX array, dimension (LDWORK,K)
.TP 8
LDWORK  (input) INTEGER
The leading dimension of the array WORK.
If SIDE = \(aqL\(aq, LDWORK >= max(1,N);
if SIDE = \(aqR\(aq, LDWORK >= max(1,M).
