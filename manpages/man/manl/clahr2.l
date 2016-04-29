.SH NAME
CLAHR2 - the first NB columns of A complex general n-BY-(n-k+1) matrix A so that elements below the k-th subdiagonal are zero
.SH SYNOPSIS
.TP 19
SUBROUTINE CLAHR2(
N, K, NB, A, LDA, TAU, T, LDT, Y, LDY )
.TP 19
.ti +4
INTEGER
K, LDA, LDT, LDY, N, NB
.TP 19
.ti +4
COMPLEX
A( LDA, * ), T( LDT, NB ), TAU( NB ),
Y( LDY, NB )
.SH PURPOSE
CLAHR2 reduces the first NB columns of A complex general n-BY-(n-k+1)
matrix A so that elements below the k-th subdiagonal are zero. The
reduction is performed by an unitary similarity transformation
Q\(aq * A * Q. The routine returns the matrices V and T which determine
Q as a block reflector I - V*T*V\(aq, and also the matrix Y = A * V * T.

This is an auxiliary routine called by CGEHRD.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.
.TP 8
K       (input) INTEGER
The offset for the reduction. Elements below the k-th
subdiagonal in the first NB columns are reduced to zero.
K < N.
.TP 8
NB      (input) INTEGER
The number of columns to be reduced.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N-K+1)
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
TAU     (output) COMPLEX array, dimension (NB)
The scalar factors of the elementary reflectors. See Further
Details.
.TP 8
T       (output) COMPLEX array, dimension (LDT,NB)
The upper triangular matrix T.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T.  LDT >= NB.
.TP 8
Y       (output) COMPLEX array, dimension (LDY,NB)
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

where tau is a complex scalar, and v is a complex vector with
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

   ( a   a   a   a   a )
.br
   ( a   a   a   a   a )
.br
   ( a   a   a   a   a )
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

This file is a slight modification of LAPACK-3.0\(aqs CLAHRD
incorporating improvements proposed by Quintana-Orti and Van de
Gejin. Note that the entries of A(1:K,2:NB) differ from those
returned by the original LAPACK routine. This function is
not backward compatible with LAPACK3.0.
.br

