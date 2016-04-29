.TH  ZGELQ2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZGELQ2 - an LQ factorization of a complex m by n matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGELQ2(
M, N, A, LDA, TAU, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, M, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
ZGELQ2 computes an LQ factorization of a complex m by n matrix A:
A = L * Q.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the m by n matrix A.
On exit, the elements on and below the diagonal of the array
contain the m by min(m,n) lower trapezoidal matrix L (L is
lower triangular if m <= n); the elements above the diagonal,
with the array TAU, represent the unitary matrix Q as a
product of elementary reflectors (see Further Details).
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
TAU     (output) COMPLEX*16 array, dimension (min(M,N))
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (M)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(k)\(aq . . . H(2)\(aq H(1)\(aq, where k = min(m,n).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a complex scalar, and v is a complex vector with
v(1:i-1) = 0 and v(i) = 1; conjg(v(i+1:n)) is stored on exit in
A(i,i+1:n), and tau in TAU(i).
.br

