.SH NAME
ZLAG2C - a DOUBLE PRECISION COMPLEX matrix, SA, to a SINGLE PRECISION COMPLEX matrix, A
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAG2C(
M, N, A, LDA, SA, LDSA, INFO)
.TP 19
.ti +4
INTEGER
INFO,LDA,LDSA,M,N
.TP 19
.ti +4
COMPLEX
SA(LDSA,*)
.TP 19
.ti +4
COMPLEX*16
A(LDA,*)
.SH PURPOSE
ZLAG2C converts a DOUBLE PRECISION COMPLEX matrix, SA, to a SINGLE
PRECISION COMPLEX matrix, A.

RMAX is the overflow for the SINGLE PRECISION arithmetic
.br
ZLAG2C checks that all the entries of A are between -RMAX and
RMAX. If not the convertion is aborted and a flag is raised.

This is a helper routine so there is no argument checking.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of lines of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the M-by-N coefficient matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
SA      (output) REAL array, dimension (LDSA,N)
On exit, if INFO=0, the M-by-N coefficient matrix SA.
.TP 8
LDSA    (input) INTEGER
The leading dimension of the array SA.  LDSA >= max(1,M).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
> 0:  if INFO = k, the (i,j) entry of the matrix A has
overflowed when moving from DOUBLE PRECISION to SINGLE
k is given by k = (i-1)*LDA+j

=========

.. Local Scalars ..

..
.. Intrinsic Functions ..

..
.. External Functions ..

..
.. Executable Statements ..

End of ZLAG2C
