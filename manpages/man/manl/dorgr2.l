.TH  DORGR2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DORGR2 - an m by n real matrix Q with orthonormal rows,
.SH SYNOPSIS
.TP 19
SUBROUTINE DORGR2(
M, N, K, A, LDA, TAU, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, K, LDA, M, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
DORGR2 generates an m by n real matrix Q with orthonormal rows,
which is defined as the last m rows of a product of k elementary
reflectors of order n
.br

      Q  =  H(1) H(2) . . . H(k)
.br

as returned by DGERQF.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix Q. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix Q. N >= M.
.TP 8
K       (input) INTEGER
The number of elementary reflectors whose product defines the
matrix Q. M >= K >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the (m-k+i)-th row must contain the vector which
defines the elementary reflector H(i), for i = 1,2,...,k, as
returned by DGERQF in the last k rows of its array argument
A.
On exit, the m by n matrix Q.
.TP 8
LDA     (input) INTEGER
The first dimension of the array A. LDA >= max(1,M).
.TP 8
TAU     (input) DOUBLE PRECISION array, dimension (K)
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by DGERQF.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (M)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument has an illegal value
