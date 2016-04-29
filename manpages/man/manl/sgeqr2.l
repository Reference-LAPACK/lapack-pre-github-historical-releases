.TH  SGEQR2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SGEQR2 - a QR factorization of a real m by n matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE SGEQR2(
M, N, A, LDA, TAU, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, M, N
.TP 19
.ti +4
REAL
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
SGEQR2 computes a QR factorization of a real m by n matrix A:
A = Q * R.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the m by n matrix A.
On exit, the elements on and above the diagonal of the array
contain the min(m,n) by n upper trapezoidal matrix R (R is
upper triangular if m >= n); the elements below the diagonal,
with the array TAU, represent the orthogonal matrix Q as a
product of elementary reflectors (see Further Details).
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
TAU     (output) REAL array, dimension (min(M,N))
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
WORK    (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(1) H(2) . . . H(k), where k = min(m,n).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a real scalar, and v is a real vector with
.br
v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
and tau in TAU(i).
.br

