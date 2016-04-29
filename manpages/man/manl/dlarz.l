.TH  DLARZ 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DLARZ - a real elementary reflector H to a real M-by-N matrix C, from either the left or the right
.SH SYNOPSIS
.TP 18
SUBROUTINE DLARZ(
SIDE, M, N, L, V, INCV, TAU, C, LDC, WORK )
.TP 18
.ti +4
CHARACTER
SIDE
.TP 18
.ti +4
INTEGER
INCV, L, LDC, M, N
.TP 18
.ti +4
DOUBLE
PRECISION TAU
.TP 18
.ti +4
DOUBLE
PRECISION C( LDC, * ), V( * ), WORK( * )
.SH PURPOSE
DLARZ applies a real elementary reflector H to a real M-by-N
matrix C, from either the left or the right. H is represented in the
form
.br

      H = I - tau * v * v\(aq
.br

where tau is a real scalar and v is a real vector.
.br

If tau = 0, then H is taken to be the unit matrix.
.br


H is a product of k elementary reflectors as returned by DTZRZF.

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= \(aqL\(aq: form  H * C
.br
= \(aqR\(aq: form  C * H
.TP 8
M       (input) INTEGER
The number of rows of the matrix C.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C.
.TP 8
L       (input) INTEGER
The number of entries of the vector V containing
the meaningful part of the Householder vectors.
If SIDE = \(aqL\(aq, M >= L >= 0, if SIDE = \(aqR\(aq, N >= L >= 0.
.TP 8
V       (input) DOUBLE PRECISION array, dimension (1+(L-1)*abs(INCV))
The vector v in the representation of H as returned by
DTZRZF. V is not used if TAU = 0.
.TP 8
INCV    (input) INTEGER
The increment between elements of v. INCV <> 0.
.TP 8
TAU     (input) DOUBLE PRECISION
The value tau in the representation of H.
.TP 8
C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by the matrix H * C if SIDE = \(aqL\(aq,
or C * H if SIDE = \(aqR\(aq.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension
(N) if SIDE = \(aqL\(aq
or (M) if SIDE = \(aqR\(aq
.SH FURTHER DETAILS
Based on contributions by
.br
  A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA

