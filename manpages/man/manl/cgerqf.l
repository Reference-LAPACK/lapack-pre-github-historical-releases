.TH  CGERQF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CGERQF - an RQ factorization of a complex M-by-N matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CGERQF(
M, N, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, M, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
CGERQF computes an RQ factorization of a complex M-by-N matrix A:
A = R * Q.
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
On entry, the M-by-N matrix A.
On exit,
if m <= n, the upper triangle of the subarray
A(1:m,n-m+1:n) contains the M-by-M upper triangular matrix R;
if m >= n, the elements on and above the (m-n)-th subdiagonal
contain the M-by-N upper trapezoidal matrix R;
the remaining elements, with the array TAU, represent the
unitary matrix Q as a product of min(m,n) elementary
reflectors (see Further Details).
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
TAU     (output) COMPLEX array, dimension (min(M,N))
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,M).
For optimum performance LWORK >= M*NB, where NB is
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
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(1)\(aq H(2)\(aq . . . H(k)\(aq, where k = min(m,n).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a complex scalar, and v is a complex vector with
v(n-k+i+1:n) = 0 and v(n-k+i) = 1; conjg(v(1:n-k+i-1)) is stored on
exit in A(m-k+i,1:n-k+i-1), and tau in TAU(i).
.br

