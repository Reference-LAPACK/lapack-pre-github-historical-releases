.TH  SSPTRD 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SSPTRD - a real symmetric matrix A stored in packed form to symmetric tridiagonal form T by an orthogonal similarity transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE SSPTRD(
UPLO, N, AP, D, E, TAU, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
REAL
AP( * ), D( * ), E( * ), TAU( * )
.SH PURPOSE
SSPTRD reduces a real symmetric matrix A stored in packed form to
symmetric tridiagonal form T by an orthogonal similarity
transformation: Q**T * A * Q = T.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
AP      (input/output) REAL array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the symmetric matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
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
< 0:  if INFO = -i, the i-th argument had an illegal value
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
v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in AP,
overwriting A(1:i-1,i+1), and tau is stored in TAU(i).
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
v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in AP,
overwriting A(i+2:n,i), and tau is stored in TAU(i).
.br

