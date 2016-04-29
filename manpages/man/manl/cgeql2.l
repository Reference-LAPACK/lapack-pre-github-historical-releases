.TH  CGEQL2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CGEQL2 - a QL factorization of a complex m by n matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CGEQL2(
M, N, A, LDA, TAU, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, M, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
CGEQL2 computes a QL factorization of a complex m by n matrix A:
A = Q * L.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the m by n matrix A.
On exit, if m >= n, the lower triangle of the subarray
A(m-n+1:m,1:n) contains the n by n lower triangular matrix L;
if m <= n, the elements on and below the (n-m)-th
superdiagonal contain the m by n lower trapezoidal matrix L;
the remaining elements, with the array TAU, represent the
unitary matrix Q as a product of elementary reflectors
(see Further Details).
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
TAU     (output) COMPLEX array, dimension (min(M,N))
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
WORK    (workspace) COMPLEX array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(k) . . . H(2) H(1), where k = min(m,n).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a complex scalar, and v is a complex vector with
v(m-k+i+1:m) = 0 and v(m-k+i) = 1; v(1:m-k+i-1) is stored on exit in
A(1:m-k+i-1,n-k+i), and tau in TAU(i).
.br

