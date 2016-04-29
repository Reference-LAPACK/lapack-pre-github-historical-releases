.TH  CUNGL2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CUNGL2 - an m-by-n complex matrix Q with orthonormal rows,
.SH SYNOPSIS
.TP 19
SUBROUTINE CUNGL2(
M, N, K, A, LDA, TAU, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, K, LDA, M, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
CUNGL2 generates an m-by-n complex matrix Q with orthonormal rows,
which is defined as the first m rows of a product of k elementary
reflectors of order n
.br

      Q  =  H(k)\(aq . . . H(2)\(aq H(1)\(aq
.br

as returned by CGELQF.
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
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the i-th row must contain the vector which defines
the elementary reflector H(i), for i = 1,2,...,k, as returned
by CGELQF in the first k rows of its array argument A.
On exit, the m by n matrix Q.
.TP 8
LDA     (input) INTEGER
The first dimension of the array A. LDA >= max(1,M).
.TP 8
TAU     (input) COMPLEX array, dimension (K)
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by CGELQF.
.TP 8
WORK    (workspace) COMPLEX array, dimension (M)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument has an illegal value
