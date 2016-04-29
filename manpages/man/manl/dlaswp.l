.TH  DLASWP 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASWP - a series of row interchanges on the matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASWP(
N, A, LDA, K1, K2, IPIV, INCX )
.TP 19
.ti +4
INTEGER
INCX, K1, K2, LDA, N
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * )
.SH PURPOSE
DLASWP performs a series of row interchanges on the matrix A.
One row interchange is initiated for each of rows K1 through K2 of A.

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the matrix of column dimension N to which the row
interchanges will be applied.
On exit, the permuted matrix.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.
.TP 8
K1      (input) INTEGER
The first element of IPIV for which a row interchange will
be done.
.TP 8
K2      (input) INTEGER
The last element of IPIV for which a row interchange will
be done.
.TP 8
IPIV    (input) INTEGER array, dimension (K2*abs(INCX))
The vector of pivot indices.  Only the elements in positions
K1 through K2 of IPIV are accessed.
IPIV(K) = L implies rows K and L are to be interchanged.
.TP 8
INCX    (input) INTEGER
The increment between successive values of IPIV.  If IPIV
is negative, the pivots are applied in reverse order.
.SH FURTHER DETAILS
Modified by
.br
 R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA

