.TH  DORMBR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DORMBR - = \(aqQ\(aq, DORMBR overwrites the general real M-by-N matrix C with  SIDE = \(aqL\(aq SIDE = \(aqR\(aq TRANS = \(aqN\(aq
.SH SYNOPSIS
.TP 19
SUBROUTINE DORMBR(
VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,
LDC, WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
SIDE, TRANS, VECT
.TP 19
.ti +4
INTEGER
INFO, K, LDA, LDC, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
.SH PURPOSE
If VECT = \(aqQ\(aq, DORMBR overwrites the general real M-by-N matrix C
with
                SIDE = \(aqL\(aq     SIDE = \(aqR\(aq
TRANS = \(aqN\(aq:      Q * C          C * Q
TRANS = \(aqT\(aq:      Q**T * C       C * Q**T
.br

If VECT = \(aqP\(aq, DORMBR overwrites the general real M-by-N matrix C
with
.br
                SIDE = \(aqL\(aq     SIDE = \(aqR\(aq
.br
TRANS = \(aqN\(aq:      P * C          C * P
.br
TRANS = \(aqT\(aq:      P**T * C       C * P**T
.br

Here Q and P**T are the orthogonal matrices determined by DGEBRD when
reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
P**T are defined as products of elementary reflectors H(i) and G(i)
respectively.
.br

Let nq = m if SIDE = \(aqL\(aq and nq = n if SIDE = \(aqR\(aq. Thus nq is the
order of the orthogonal matrix Q or P**T that is applied.

If VECT = \(aqQ\(aq, A is assumed to have been an NQ-by-K matrix:
if nq >= k, Q = H(1) H(2) . . . H(k);
.br
if nq < k, Q = H(1) H(2) . . . H(nq-1).
.br

If VECT = \(aqP\(aq, A is assumed to have been a K-by-NQ matrix:
if k < nq, P = G(1) G(2) . . . G(k);
.br
if k >= nq, P = G(1) G(2) . . . G(nq-1).
.br

.SH ARGUMENTS
.TP 8
VECT    (input) CHARACTER*1
= \(aqQ\(aq: apply Q or Q**T;
.br
= \(aqP\(aq: apply P or P**T.
.TP 8
SIDE    (input) CHARACTER*1
.br
= \(aqL\(aq: apply Q, Q**T, P or P**T from the Left;
.br
= \(aqR\(aq: apply Q, Q**T, P or P**T from the Right.
.TP 8
TRANS   (input) CHARACTER*1
.br
= \(aqN\(aq:  No transpose, apply Q  or P;
.br
= \(aqT\(aq:  Transpose, apply Q**T or P**T.
.TP 8
M       (input) INTEGER
The number of rows of the matrix C. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C. N >= 0.
.TP 8
K       (input) INTEGER
If VECT = \(aqQ\(aq, the number of columns in the original
matrix reduced by DGEBRD.
If VECT = \(aqP\(aq, the number of rows in the original
matrix reduced by DGEBRD.
K >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension
(LDA,min(nq,K)) if VECT = \(aqQ\(aq
(LDA,nq)        if VECT = \(aqP\(aq
The vectors which define the elementary reflectors H(i) and
G(i), whose products determine the matrices Q and P, as
returned by DGEBRD.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.
If VECT = \(aqQ\(aq, LDA >= max(1,nq);
if VECT = \(aqP\(aq, LDA >= max(1,min(nq,K)).
.TP 8
TAU     (input) DOUBLE PRECISION array, dimension (min(nq,K))
TAU(i) must contain the scalar factor of the elementary
reflector H(i) or G(i) which determines Q or P, as returned
by DGEBRD in the array argument TAUQ or TAUP.
.TP 8
C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
or P*C or P**T*C or C*P or C*P**T.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If SIDE = \(aqL\(aq, LWORK >= max(1,N);
if SIDE = \(aqR\(aq, LWORK >= max(1,M).
For optimum performance LWORK >= N*NB if SIDE = \(aqL\(aq, and
LWORK >= M*NB if SIDE = \(aqR\(aq, where NB is the optimal
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
