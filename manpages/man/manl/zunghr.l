.TH  ZUNGHR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZUNGHR - a complex unitary matrix Q which is defined as the product of IHI-ILO elementary reflectors of order N, as returned by ZGEHRD
.SH SYNOPSIS
.TP 19
SUBROUTINE ZUNGHR(
N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LWORK, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
ZUNGHR generates a complex unitary matrix Q which is defined as the
product of IHI-ILO elementary reflectors of order N, as returned by
ZGEHRD:

Q = H(ilo) H(ilo+1) . . . H(ihi-1).
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix Q. N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
ILO and IHI must have the same values as in the previous call
of ZGEHRD. Q is equal to the unit matrix except in the
submatrix Q(ilo+1:ihi,ilo+1:ihi).
1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the vectors which define the elementary reflectors,
as returned by ZGEHRD.
On exit, the N-by-N unitary matrix Q.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
TAU     (input) COMPLEX*16 array, dimension (N-1)
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by ZGEHRD.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= IHI-ILO.
For optimum performance LWORK >= (IHI-ILO)*NB, where NB is
the optimal blocksize.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
