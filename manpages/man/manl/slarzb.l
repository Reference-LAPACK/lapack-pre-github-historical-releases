.TH  SLARZB 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLARZB - a real block reflector H or its transpose H**T to a real distributed M-by-N C from the left or the right
.SH SYNOPSIS
.TP 19
SUBROUTINE SLARZB(
SIDE, TRANS, DIRECT, STOREV, M, N, K, L, V,
LDV, T, LDT, C, LDC, WORK, LDWORK )
.TP 19
.ti +4
CHARACTER
DIRECT, SIDE, STOREV, TRANS
.TP 19
.ti +4
INTEGER
K, L, LDC, LDT, LDV, LDWORK, M, N
.TP 19
.ti +4
REAL
C( LDC, * ), T( LDT, * ), V( LDV, * ),
WORK( LDWORK, * )
.SH PURPOSE
SLARZB applies a real block reflector H or its transpose H**T to
a real distributed M-by-N  C from the left or the right.

Currently, only STOREV = \(aqR\(aq and DIRECT = \(aqB\(aq are supported.

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
= \(aqC\(aq: apply H\(aq (Transpose)
.TP 8
DIRECT  (input) CHARACTER*1
Indicates how H is formed from a product of elementary
reflectors
= \(aqF\(aq: H = H(1) H(2) . . . H(k) (Forward, not supported yet)
.br
= \(aqB\(aq: H = H(k) . . . H(2) H(1) (Backward)
.TP 8
STOREV  (input) CHARACTER*1
Indicates how the vectors which define the elementary
reflectors are stored:
.br
= \(aqC\(aq: Columnwise                        (not supported yet)
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
L       (input) INTEGER
The number of columns of the matrix V containing the
meaningful part of the Householder reflectors.
If SIDE = \(aqL\(aq, M >= L >= 0, if SIDE = \(aqR\(aq, N >= L >= 0.
.TP 8
V       (input) REAL array, dimension (LDV,NV).
If STOREV = \(aqC\(aq, NV = K; if STOREV = \(aqR\(aq, NV = L.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V.
If STOREV = \(aqC\(aq, LDV >= L; if STOREV = \(aqR\(aq, LDV >= K.
.TP 8
T       (input) REAL array, dimension (LDT,K)
The triangular K-by-K matrix T in the representation of the
block reflector.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= K.
.TP 8
C       (input/output) REAL array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by H*C or H\(aq*C or C*H or C*H\(aq.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace) REAL array, dimension (LDWORK,K)
.TP 8
LDWORK  (input) INTEGER
The leading dimension of the array WORK.
If SIDE = \(aqL\(aq, LDWORK >= max(1,N);
if SIDE = \(aqR\(aq, LDWORK >= max(1,M).
.SH FURTHER DETAILS
Based on contributions by
.br
  A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA

