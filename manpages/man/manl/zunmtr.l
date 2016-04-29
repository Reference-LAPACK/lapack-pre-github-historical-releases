.TH  ZUNMTR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZUNMTR - the general complex M-by-N matrix C with   SIDE = \(aqL\(aq SIDE = \(aqR\(aq TRANS = \(aqN\(aq
.SH SYNOPSIS
.TP 19
SUBROUTINE ZUNMTR(
SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,
WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
SIDE, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LDC, LWORK, M, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
.SH PURPOSE
ZUNMTR overwrites the general complex M-by-N matrix C with
TRANS = \(aqC\(aq:      Q**H * C       C * Q**H
.br

where Q is a complex unitary matrix of order nq, with nq = m if
SIDE = \(aqL\(aq and nq = n if SIDE = \(aqR\(aq. Q is defined as the product of
nq-1 elementary reflectors, as returned by ZHETRD:
.br

if UPLO = \(aqU\(aq, Q = H(nq-1) . . . H(2) H(1);
.br

if UPLO = \(aqL\(aq, Q = H(1) H(2) . . . H(nq-1).
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= \(aqL\(aq: apply Q or Q**H from the Left;
.br
= \(aqR\(aq: apply Q or Q**H from the Right.
.TP 8
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq: Upper triangle of A contains elementary reflectors
from ZHETRD;
= \(aqL\(aq: Lower triangle of A contains elementary reflectors
from ZHETRD.
.TP 8
TRANS   (input) CHARACTER*1
= \(aqN\(aq:  No transpose, apply Q;
.br
= \(aqC\(aq:  Conjugate transpose, apply Q**H.
.TP 8
M       (input) INTEGER
The number of rows of the matrix C. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C. N >= 0.
.TP 8
A       (input) COMPLEX*16 array, dimension
(LDA,M) if SIDE = \(aqL\(aq
(LDA,N) if SIDE = \(aqR\(aq
The vectors which define the elementary reflectors, as
returned by ZHETRD.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.
LDA >= max(1,M) if SIDE = \(aqL\(aq; LDA >= max(1,N) if SIDE = \(aqR\(aq.
.TP 8
TAU     (input) COMPLEX*16 array, dimension
(M-1) if SIDE = \(aqL\(aq
(N-1) if SIDE = \(aqR\(aq
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by ZHETRD.
.TP 8
C       (input/output) COMPLEX*16 array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If SIDE = \(aqL\(aq, LWORK >= max(1,N);
if SIDE = \(aqR\(aq, LWORK >= max(1,M).
For optimum performance LWORK >= N*NB if SIDE = \(aqL\(aq, and
LWORK >=M*NB if SIDE = \(aqR\(aq, where NB is the optimal
blocksize.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
