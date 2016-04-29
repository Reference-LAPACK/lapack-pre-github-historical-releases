.TH  SLATRD 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLATRD - NB rows and columns of a real symmetric matrix A to symmetric tridiagonal form by an orthogonal similarity transformation Q\(aq * A * Q, and returns the matrices V and W which are needed to apply the transformation to the unreduced part of A
.SH SYNOPSIS
.TP 19
SUBROUTINE SLATRD(
UPLO, N, NB, A, LDA, E, TAU, W, LDW )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
LDA, LDW, N, NB
.TP 19
.ti +4
REAL
A( LDA, * ), E( * ), TAU( * ), W( LDW, * )
.SH PURPOSE
SLATRD reduces NB rows and columns of a real symmetric matrix A to
symmetric tridiagonal form by an orthogonal similarity
transformation Q\(aq * A * Q, and returns the matrices V and W which are
needed to apply the transformation to the unreduced part of A.

If UPLO = \(aqU\(aq, SLATRD reduces the last NB rows and columns of a
matrix, of which the upper triangle is supplied;
.br
if UPLO = \(aqL\(aq, SLATRD reduces the first NB rows and columns of a
matrix, of which the lower triangle is supplied.
.br

This is an auxiliary routine called by SSYTRD.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is stored:
.br
= \(aqU\(aq: Upper triangular
.br
= \(aqL\(aq: Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.
.TP 8
NB      (input) INTEGER
The number of rows and columns to be reduced.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the leading
n-by-n upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading n-by-n lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.
On exit:
if UPLO = \(aqU\(aq, the last NB columns have been reduced to
tridiagonal form, with the diagonal elements overwriting
the diagonal elements of A; the elements above the diagonal
with the array TAU, represent the orthogonal matrix Q as a
product of elementary reflectors;
if UPLO = \(aqL\(aq, the first NB columns have been reduced to
tridiagonal form, with the diagonal elements overwriting
the diagonal elements of A; the elements below the diagonal
with the array TAU, represent the  orthogonal matrix Q as a
product of elementary reflectors.
See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= (1,N).
.TP 8
E       (output) REAL array, dimension (N-1)
If UPLO = \(aqU\(aq, E(n-nb:n-1) contains the superdiagonal
elements of the last NB columns of the reduced matrix;
if UPLO = \(aqL\(aq, E(1:nb) contains the subdiagonal elements of
the first NB columns of the reduced matrix.
.TP 8
TAU     (output) REAL array, dimension (N-1)
The scalar factors of the elementary reflectors, stored in
TAU(n-nb:n-1) if UPLO = \(aqU\(aq, and in TAU(1:nb) if UPLO = \(aqL\(aq.
See Further Details.
W       (output) REAL array, dimension (LDW,NB)
The n-by-nb matrix W required to update the unreduced part
of A.
.TP 8
LDW     (input) INTEGER
The leading dimension of the array W. LDW >= max(1,N).
.SH FURTHER DETAILS
If UPLO = \(aqU\(aq, the matrix Q is represented as a product of elementary
reflectors
.br

   Q = H(n) H(n-1) . . . H(n-nb+1).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a real scalar, and v is a real vector with
.br
v(i:n) = 0 and v(i-1) = 1; v(1:i-1) is stored on exit in A(1:i-1,i),
and tau in TAU(i-1).
.br

If UPLO = \(aqL\(aq, the matrix Q is represented as a product of elementary
reflectors
.br

   Q = H(1) H(2) . . . H(nb).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a real scalar, and v is a real vector with
.br
v(1:i) = 0 and v(i+1) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
and tau in TAU(i).
.br

The elements of the vectors v together form the n-by-nb matrix V
which is needed, with W, to apply the transformation to the unreduced
part of the matrix, using a symmetric rank-2k update of the form:
A := A - V*W\(aq - W*V\(aq.
.br

The contents of A on exit are illustrated by the following examples
with n = 5 and nb = 2:
.br

if UPLO = \(aqU\(aq:                       if UPLO = \(aqL\(aq:
.br

  (  a   a   a   v4  v5 )              (  d                  )
  (      a   a   v4  v5 )              (  1   d              )
  (          a   1   v5 )              (  v1  1   a          )
  (              d   1  )              (  v1  v2  a   a      )
  (                  d  )              (  v1  v2  a   a   a  )

where d denotes a diagonal element of the reduced matrix, a denotes
an element of the original matrix that is unchanged, and vi denotes
an element of the vector defining H(i).
.br

