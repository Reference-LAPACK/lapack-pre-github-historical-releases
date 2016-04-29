.TH  SLAHRD 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAHRD - the first NB columns of a real general n-by-(n-k+1) matrix A so that elements below the k-th subdiagonal are zero
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAHRD(
N, K, NB, A, LDA, TAU, T, LDT, Y, LDY )
.TP 19
.ti +4
INTEGER
K, LDA, LDT, LDY, N, NB
.TP 19
.ti +4
REAL
A( LDA, * ), T( LDT, NB ), TAU( NB ),
Y( LDY, NB )
.SH PURPOSE
SLAHRD reduces the first NB columns of a real general n-by-(n-k+1)
matrix A so that elements below the k-th subdiagonal are zero. The
reduction is performed by an orthogonal similarity transformation
Q\(aq * A * Q. The routine returns the matrices V and T which determine
Q as a block reflector I - V*T*V\(aq, and also the matrix Y = A * V * T.

This is an OBSOLETE auxiliary routine. 
.br
This routine will be \(aqdeprecated\(aq in a  future release.
.br
Please use the new routine SLAHR2 instead.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.
.TP 8
K       (input) INTEGER
The offset for the reduction. Elements below the k-th
subdiagonal in the first NB columns are reduced to zero.
.TP 8
NB      (input) INTEGER
The number of columns to be reduced.
.TP 8
A       (input/output) REAL array, dimension (LDA,N-K+1)
On entry, the n-by-(n-k+1) general matrix A.
On exit, the elements on and above the k-th subdiagonal in
the first NB columns are overwritten with the corresponding
elements of the reduced matrix; the elements below the k-th
subdiagonal, with the array TAU, represent the matrix Q as a
product of elementary reflectors. The other columns of A are
unchanged. See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
TAU     (output) REAL array, dimension (NB)
The scalar factors of the elementary reflectors. See Further
Details.
.TP 8
T       (output) REAL array, dimension (LDT,NB)
The upper triangular matrix T.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T.  LDT >= NB.
.TP 8
Y       (output) REAL array, dimension (LDY,NB)
The n-by-nb matrix Y.
.TP 8
LDY     (input) INTEGER
The leading dimension of the array Y. LDY >= N.
.SH FURTHER DETAILS
The matrix Q is represented as a product of nb elementary reflectors

   Q = H(1) H(2) . . . H(nb).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a real scalar, and v is a real vector with
.br
v(1:i+k-1) = 0, v(i+k) = 1; v(i+k+1:n) is stored on exit in
A(i+k+1:n,i), and tau in TAU(i).
.br

The elements of the vectors v together form the (n-k+1)-by-nb matrix
V which is needed, with T and Y, to apply the transformation to the
unreduced part of the matrix, using an update of the form:
A := (I - V*T*V\(aq) * (A - Y*V\(aq).
.br

The contents of A on exit are illustrated by the following example
with n = 7, k = 3 and nb = 2:
.br

   ( a   h   a   a   a )
.br
   ( a   h   a   a   a )
.br
   ( a   h   a   a   a )
.br
   ( h   h   a   a   a )
.br
   ( v1  h   a   a   a )
.br
   ( v1  v2  a   a   a )
.br
   ( v1  v2  a   a   a )
.br

where a denotes an element of the original matrix A, h denotes a
modified element of the upper Hessenberg matrix H, and vi denotes an
element of the vector defining H(i).
.br

