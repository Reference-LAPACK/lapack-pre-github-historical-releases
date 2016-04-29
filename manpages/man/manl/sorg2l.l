.TH  SORG2L 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SORG2L - an m by n real matrix Q with orthonormal columns,
.SH SYNOPSIS
.TP 19
SUBROUTINE SORG2L(
M, N, K, A, LDA, TAU, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, K, LDA, M, N
.TP 19
.ti +4
REAL
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
SORG2L generates an m by n real matrix Q with orthonormal columns,
which is defined as the last n columns of a product of k elementary
reflectors of order m
.br

      Q  =  H(k) . . . H(2) H(1)
.br

as returned by SGEQLF.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix Q. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix Q. M >= N >= 0.
.TP 8
K       (input) INTEGER
The number of elementary reflectors whose product defines the
matrix Q. N >= K >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the (n-k+i)-th column must contain the vector which
defines the elementary reflector H(i), for i = 1,2,...,k, as
returned by SGEQLF in the last k columns of its array
argument A.
On exit, the m by n matrix Q.
.TP 8
LDA     (input) INTEGER
The first dimension of the array A. LDA >= max(1,M).
.TP 8
TAU     (input) REAL array, dimension (K)
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by SGEQLF.
.TP 8
WORK    (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument has an illegal value
