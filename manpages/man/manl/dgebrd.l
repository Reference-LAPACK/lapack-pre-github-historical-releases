.TH  DGEBRD 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DGEBRD - a general real M-by-N matrix A to upper or lower bidiagonal form B by an orthogonal transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE DGEBRD(
M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,
INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), D( * ), E( * ), TAUP( * ),
TAUQ( * ), WORK( * )
.SH PURPOSE
DGEBRD reduces a general real M-by-N matrix A to upper or lower
bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.

If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows in the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns in the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the M-by-N general matrix to be reduced.
On exit,
if m >= n, the diagonal and the first superdiagonal are
overwritten with the upper bidiagonal matrix B; the
elements below the diagonal, with the array TAUQ, represent
the orthogonal matrix Q as a product of elementary
reflectors, and the elements above the first superdiagonal,
with the array TAUP, represent the orthogonal matrix P as
a product of elementary reflectors;
if m < n, the diagonal and the first subdiagonal are
overwritten with the lower bidiagonal matrix B; the
elements below the first subdiagonal, with the array TAUQ,
represent the orthogonal matrix Q as a product of
elementary reflectors, and the elements above the diagonal,
with the array TAUP, represent the orthogonal matrix P as
a product of elementary reflectors.
See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
D       (output) DOUBLE PRECISION array, dimension (min(M,N))
The diagonal elements of the bidiagonal matrix B:
D(i) = A(i,i).
.TP 8
E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
The off-diagonal elements of the bidiagonal matrix B:
if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
.TP 8
TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
The scalar factors of the elementary reflectors which
represent the orthogonal matrix Q. See Further Details.
TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
The scalar factors of the elementary reflectors which
represent the orthogonal matrix P. See Further Details.
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of the array WORK.  LWORK >= max(1,M,N).
For optimum performance LWORK >= (M+N)*NB, where NB
is the optimal blocksize.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
The matrices Q and P are represented as products of elementary
reflectors:
.br

If m >= n,
.br

   Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)

Each H(i) and G(i) has the form:
.br

   H(i) = I - tauq * v * v\(aq  and G(i) = I - taup * u * u\(aq

where tauq and taup are real scalars, and v and u are real vectors;
v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
tauq is stored in TAUQ(i) and taup in TAUP(i).
.br

If m < n,
.br

   Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)

Each H(i) and G(i) has the form:
.br

   H(i) = I - tauq * v * v\(aq  and G(i) = I - taup * u * u\(aq

where tauq and taup are real scalars, and v and u are real vectors;
v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
tauq is stored in TAUQ(i) and taup in TAUP(i).
.br

The contents of A on exit are illustrated by the following examples:

m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):

  (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
  (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
  (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
  (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
  (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
  (  v1  v2  v3  v4  v5 )
.br

where d and e denote diagonal and off-diagonal elements of B, vi
denotes an element of the vector defining H(i), and ui an element of
the vector defining G(i).
.br

