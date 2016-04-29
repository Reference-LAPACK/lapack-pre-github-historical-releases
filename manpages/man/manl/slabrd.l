.TH  SLABRD 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLABRD - the first NB rows and columns of a real general m by n matrix A to upper or lower bidiagonal form by an orthogonal transformation Q\(aq * A * P, and returns the matrices X and Y which are needed to apply the transformation to the unreduced part of A
.SH SYNOPSIS
.TP 19
SUBROUTINE SLABRD(
M, N, NB, A, LDA, D, E, TAUQ, TAUP, X, LDX, Y,
LDY )
.TP 19
.ti +4
INTEGER
LDA, LDX, LDY, M, N, NB
.TP 19
.ti +4
REAL
A( LDA, * ), D( * ), E( * ), TAUP( * ),
TAUQ( * ), X( LDX, * ), Y( LDY, * )
.SH PURPOSE
SLABRD reduces the first NB rows and columns of a real general
m by n matrix A to upper or lower bidiagonal form by an orthogonal
transformation Q\(aq * A * P, and returns the matrices X and Y which
are needed to apply the transformation to the unreduced part of A.

If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
bidiagonal form.
.br

This is an auxiliary routine called by SGEBRD
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows in the matrix A.
.TP 8
N       (input) INTEGER
The number of columns in the matrix A.
.TP 8
NB      (input) INTEGER
The number of leading rows and columns of A to be reduced.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the m by n general matrix to be reduced.
On exit, the first NB rows and columns of the matrix are
overwritten; the rest of the array is unchanged.
If m >= n, elements on and below the diagonal in the first NB
columns, with the array TAUQ, represent the orthogonal
matrix Q as a product of elementary reflectors; and
elements above the diagonal in the first NB rows, with the
array TAUP, represent the orthogonal matrix P as a product
of elementary reflectors.
If m < n, elements below the diagonal in the first NB
columns, with the array TAUQ, represent the orthogonal
matrix Q as a product of elementary reflectors, and
elements on and above the diagonal in the first NB rows,
with the array TAUP, represent the orthogonal matrix P as
a product of elementary reflectors.
See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
D       (output) REAL array, dimension (NB)
The diagonal elements of the first NB rows and columns of
the reduced matrix.  D(i) = A(i,i).
.TP 8
E       (output) REAL array, dimension (NB)
The off-diagonal elements of the first NB rows and columns of
the reduced matrix.
.TP 8
TAUQ    (output) REAL array dimension (NB)
The scalar factors of the elementary reflectors which
represent the orthogonal matrix Q. See Further Details.
TAUP    (output) REAL array, dimension (NB)
The scalar factors of the elementary reflectors which
represent the orthogonal matrix P. See Further Details.
X       (output) REAL array, dimension (LDX,NB)
The m-by-nb matrix X required to update the unreduced part
of A.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X. LDX >= M.
.TP 8
Y       (output) REAL array, dimension (LDY,NB)
The n-by-nb matrix Y required to update the unreduced part
of A.
.TP 8
LDY     (input) INTEGER
The leading dimension of the array Y. LDY >= N.
.SH FURTHER DETAILS
The matrices Q and P are represented as products of elementary
reflectors:
.br

   Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)

Each H(i) and G(i) has the form:
.br

   H(i) = I - tauq * v * v\(aq  and G(i) = I - taup * u * u\(aq

where tauq and taup are real scalars, and v and u are real vectors.

If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).

If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).

The elements of the vectors v and u together form the m-by-nb matrix
V and the nb-by-n matrix U\(aq which are needed, with X and Y, to apply
the transformation to the unreduced part of the matrix, using a block
update of the form:  A := A - V*Y\(aq - X*U\(aq.
.br

The contents of A on exit are illustrated by the following examples
with nb = 2:
.br

m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):

  (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
  (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
  (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
  (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
  (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
  (  v1  v2  a   a   a  )
.br

where a denotes an element of the original matrix which is unchanged,
vi denotes an element of the vector defining H(i), and ui an element
of the vector defining G(i).
.br

