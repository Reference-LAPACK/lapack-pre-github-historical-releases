.TH  ZLAQP2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLAQP2 - a QR factorization with column pivoting of the block A(OFFSET+1:M,1:N)
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAQP2(
M, N, OFFSET, A, LDA, JPVT, TAU, VN1, VN2,
WORK )
.TP 19
.ti +4
INTEGER
LDA, M, N, OFFSET
.TP 19
.ti +4
INTEGER
JPVT( * )
.TP 19
.ti +4
DOUBLE
PRECISION VN1( * ), VN2( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
ZLAQP2 computes a QR factorization with column pivoting of
the block A(OFFSET+1:M,1:N).
The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A. N >= 0.
.TP 8
OFFSET  (input) INTEGER
The number of rows of the matrix A that must be pivoted
but no factorized. OFFSET >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, the upper triangle of block A(OFFSET+1:M,1:N) is
the triangular factor obtained; the elements in block
A(OFFSET+1:M,1:N) below the diagonal, together with the
array TAU, represent the orthogonal matrix Q as a product of
elementary reflectors. Block A(1:OFFSET,1:N) has been
accordingly pivoted, but no factorized.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
JPVT    (input/output) INTEGER array, dimension (N)
On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
to the front of A*P (a leading column); if JPVT(i) = 0,
the i-th column of A is a free column.
On exit, if JPVT(i) = k, then the i-th column of A*P
was the k-th column of A.
.TP 8
TAU     (output) COMPLEX*16 array, dimension (min(M,N))
The scalar factors of the elementary reflectors.
.TP 8
VN1     (input/output) DOUBLE PRECISION array, dimension (N)
The vector with the partial column norms.
.TP 8
VN2     (input/output) DOUBLE PRECISION array, dimension (N)
The vector with the exact column norms.
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (N)
.SH FURTHER DETAILS
Based on contributions by
.br
  G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
  X. Sun, Computer Science Dept., Duke University, USA
.br

Partial column norm updating strategy modified by
.br
  Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
.br
  University of Zagreb, Croatia.
.br
  June 2006.
.br
For more details see LAPACK Working Note 176.
.br
