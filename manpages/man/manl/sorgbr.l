.TH  SORGBR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SORGBR - one of the real orthogonal matrices Q or P**T determined by SGEBRD when reducing a real matrix A to bidiagonal form
.SH SYNOPSIS
.TP 19
SUBROUTINE SORGBR(
VECT, M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
VECT
.TP 19
.ti +4
INTEGER
INFO, K, LDA, LWORK, M, N
.TP 19
.ti +4
REAL
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
SORGBR generates one of the real orthogonal matrices Q or P**T
determined by SGEBRD when reducing a real matrix A to bidiagonal
form: A = Q * B * P**T.  Q and P**T are defined as products of
elementary reflectors H(i) or G(i) respectively.
.br

If VECT = \(aqQ\(aq, A is assumed to have been an M-by-K matrix, and Q
is of order M:
.br
if m >= k, Q = H(1) H(2) . . . H(k) and SORGBR returns the first n
columns of Q, where m >= n >= k;
.br
if m < k, Q = H(1) H(2) . . . H(m-1) and SORGBR returns Q as an
M-by-M matrix.
.br

If VECT = \(aqP\(aq, A is assumed to have been a K-by-N matrix, and P**T
is of order N:
.br
if k < n, P**T = G(k) . . . G(2) G(1) and SORGBR returns the first m
rows of P**T, where n >= m >= k;
.br
if k >= n, P**T = G(n-1) . . . G(2) G(1) and SORGBR returns P**T as
an N-by-N matrix.
.br

.SH ARGUMENTS
.TP 8
VECT    (input) CHARACTER*1
Specifies whether the matrix Q or the matrix P**T is
required, as defined in the transformation applied by SGEBRD:
.br
= \(aqQ\(aq:  generate Q;
.br
= \(aqP\(aq:  generate P**T.
.TP 8
M       (input) INTEGER
The number of rows of the matrix Q or P**T to be returned.
M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix Q or P**T to be returned.
N >= 0.
If VECT = \(aqQ\(aq, M >= N >= min(M,K);
if VECT = \(aqP\(aq, N >= M >= min(N,K).
.TP 8
K       (input) INTEGER
If VECT = \(aqQ\(aq, the number of columns in the original M-by-K
matrix reduced by SGEBRD.
If VECT = \(aqP\(aq, the number of rows in the original K-by-N
matrix reduced by SGEBRD.
K >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the vectors which define the elementary reflectors,
as returned by SGEBRD.
On exit, the M-by-N matrix Q or P**T.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
TAU     (input) REAL array, dimension
(min(M,K)) if VECT = \(aqQ\(aq
(min(N,K)) if VECT = \(aqP\(aq
TAU(i) must contain the scalar factor of the elementary
reflector H(i) or G(i), which determines Q or P**T, as
returned by SGEBRD in its array argument TAUQ or TAUP.
.TP 8
WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,min(M,N)).
For optimum performance LWORK >= min(M,N)*NB, where NB
is the optimal blocksize.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
