.TH  SGEQP3 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SGEQP3 - a QR factorization with column pivoting of a matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE SGEQP3(
M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, M, N
.TP 19
.ti +4
INTEGER
JPVT( * )
.TP 19
.ti +4
REAL
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
SGEQP3 computes a QR factorization with column pivoting of a
matrix A:  A*P = Q*R  using Level 3 BLAS.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, the upper triangle of the array contains the
min(M,N)-by-N upper trapezoidal matrix R; the elements below
the diagonal, together with the array TAU, represent the
orthogonal matrix Q as a product of min(M,N) elementary
reflectors.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
JPVT    (input/output) INTEGER array, dimension (N)
On entry, if JPVT(J).ne.0, the J-th column of A is permuted
to the front of A*P (a leading column); if JPVT(J)=0,
the J-th column of A is a free column.
On exit, if JPVT(J)=K, then the J-th column of A*P was the
the K-th column of A.
.TP 8
TAU     (output) REAL array, dimension (min(M,N))
The scalar factors of the elementary reflectors.
.TP 8
WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
On exit, if INFO=0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= 3*N+1.
For optimal performance LWORK >= 2*N+( N+1 )*NB, where NB
is the optimal blocksize.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0: successful exit.
.br
< 0: if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(1) H(2) . . . H(k), where k = min(m,n).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a real/complex scalar, and v is a real/complex vector
with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
A(i+1:m,i), and tau in TAU(i).
.br

Based on contributions by
.br
  G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
  X. Sun, Computer Science Dept., Duke University, USA
.br

