.TH  ZUNGQR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZUNGQR - an M-by-N complex matrix Q with orthonormal columns,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZUNGQR(
M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, K, LDA, LWORK, M, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
which is defined as the first N columns of a product of K elementary
reflectors of order M
.br

      Q  =  H(1) H(2) . . . H(k)
.br

as returned by ZGEQRF.
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
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the i-th column must contain the vector which
defines the elementary reflector H(i), for i = 1,2,...,k, as
returned by ZGEQRF in the first k columns of its array
argument A.
On exit, the M-by-N matrix Q.
.TP 8
LDA     (input) INTEGER
The first dimension of the array A. LDA >= max(1,M).
.TP 8
TAU     (input) COMPLEX*16 array, dimension (K)
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by ZGEQRF.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,N).
For optimum performance LWORK >= N*NB, where NB is the
optimal blocksize.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument has an illegal value
