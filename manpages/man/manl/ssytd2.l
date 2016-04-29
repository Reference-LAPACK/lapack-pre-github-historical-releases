.TH  SSYTD2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SSYTD2 - a real symmetric matrix A to symmetric tridiagonal form T by an orthogonal similarity transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE SSYTD2(
UPLO, N, A, LDA, D, E, TAU, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
REAL
A( LDA, * ), D( * ), E( * ), TAU( * )
.SH PURPOSE
SSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
form T by an orthogonal similarity transformation: Q\(aq * A * Q = T.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is stored:
.br
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the leading
n-by-n upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading n-by-n lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.
On exit, if UPLO = \(aqU\(aq, the diagonal and first superdiagonal
of A are overwritten by the corresponding elements of the
tridiagonal matrix T, and the elements above the first
superdiagonal, with the array TAU, represent the orthogonal
matrix Q as a product of elementary reflectors; if UPLO
= \(aqL\(aq, the diagonal and first subdiagonal of A are over-
written by the corresponding elements of the tridiagonal
matrix T, and the elements below the first subdiagonal, with
the array TAU, represent the orthogonal matrix Q as a product
of elementary reflectors. See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
D       (output) REAL array, dimension (N)
The diagonal elements of the tridiagonal matrix T:
D(i) = A(i,i).
.TP 8
E       (output) REAL array, dimension (N-1)
The off-diagonal elements of the tridiagonal matrix T:
E(i) = A(i,i+1) if UPLO = \(aqU\(aq, E(i) = A(i+1,i) if UPLO = \(aqL\(aq.
.TP 8
TAU     (output) REAL array, dimension (N-1)
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
If UPLO = \(aqU\(aq, the matrix Q is represented as a product of elementary
reflectors
.br

   Q = H(n-1) . . . H(2) H(1).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a real scalar, and v is a real vector with
.br
v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
.br
A(1:i-1,i+1), and tau in TAU(i).
.br

If UPLO = \(aqL\(aq, the matrix Q is represented as a product of elementary
reflectors
.br

   Q = H(1) H(2) . . . H(n-1).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a real scalar, and v is a real vector with
.br
v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
and tau in TAU(i).
.br

The contents of A on exit are illustrated by the following examples
with n = 5:
.br

if UPLO = \(aqU\(aq:                       if UPLO = \(aqL\(aq:
.br

  (  d   e   v2  v3  v4 )              (  d                  )
  (      d   e   v3  v4 )              (  e   d              )
  (          d   e   v4 )              (  v1  e   d          )
  (              d   e  )              (  v1  v2  e   d      )
  (                  d  )              (  v1  v2  v3  e   d  )

where d and e denote diagonal and off-diagonal elements of T, and vi
denotes an element of the vector defining H(i).
.br

