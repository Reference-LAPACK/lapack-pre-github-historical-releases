.TH  ZSYMV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZSYMV - the matrix-vector operation   y := alpha*A*x + beta*y,
.SH SYNOPSIS
.TP 18
SUBROUTINE ZSYMV(
UPLO, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY )
.TP 18
.ti +4
CHARACTER
UPLO
.TP 18
.ti +4
INTEGER
INCX, INCY, LDA, N
.TP 18
.ti +4
COMPLEX*16
ALPHA, BETA
.TP 18
.ti +4
COMPLEX*16
A( LDA, * ), X( * ), Y( * )
.SH PURPOSE
ZSYMV  performs the matrix-vector  operation

where alpha and beta are scalars, x and y are n element vectors and
A is an n by n symmetric matrix.
.br

.SH ARGUMENTS
.TP 9
UPLO     (input) CHARACTER*1
On entry, UPLO specifies whether the upper or lower
triangular part of the array A is to be referenced as
follows:

UPLO = \(aqU\(aq or \(aqu\(aq   Only the upper triangular part of A
is to be referenced.

UPLO = \(aqL\(aq or \(aql\(aq   Only the lower triangular part of A
is to be referenced.

Unchanged on exit.
.TP 9
N        (input) INTEGER
On entry, N specifies the order of the matrix A.
N must be at least zero.
Unchanged on exit.
.TP 9
ALPHA    (input) COMPLEX*16
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 9
A        (input) COMPLEX*16 array, dimension ( LDA, N )
Before entry, with  UPLO = \(aqU\(aq or \(aqu\(aq, the leading n by n
upper triangular part of the array A must contain the upper
triangular part of the symmetric matrix and the strictly
lower triangular part of A is not referenced.
Before entry, with UPLO = \(aqL\(aq or \(aql\(aq, the leading n by n
lower triangular part of the array A must contain the lower
triangular part of the symmetric matrix and the strictly
upper triangular part of A is not referenced.
Unchanged on exit.
.TP 9
LDA      (input) INTEGER
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. LDA must be at least
max( 1, N ).
Unchanged on exit.
.TP 9
X        (input) COMPLEX*16 array, dimension at least
( 1 + ( N - 1 )*abs( INCX ) ).
Before entry, the incremented array X must contain the N-
element vector x.
Unchanged on exit.
.TP 9
INCX     (input) INTEGER
On entry, INCX specifies the increment for the elements of
X. INCX must not be zero.
Unchanged on exit.
.TP 9
BETA     (input) COMPLEX*16
On entry, BETA specifies the scalar beta. When BETA is
supplied as zero then Y need not be set on input.
Unchanged on exit.
.TP 9
Y        (input/output) COMPLEX*16 array, dimension at least
( 1 + ( N - 1 )*abs( INCY ) ).
Before entry, the incremented array Y must contain the n
element vector y. On exit, Y is overwritten by the updated
vector y.
.TP 9
INCY     (input) INTEGER
On entry, INCY specifies the increment for the elements of
Y. INCY must not be zero.
Unchanged on exit.
