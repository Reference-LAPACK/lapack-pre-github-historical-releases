.TH  ZSPMV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZSPMV - the matrix-vector operation   y := alpha*A*x + beta*y,
.SH SYNOPSIS
.TP 18
SUBROUTINE ZSPMV(
UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY )
.TP 18
.ti +4
CHARACTER
UPLO
.TP 18
.ti +4
INTEGER
INCX, INCY, N
.TP 18
.ti +4
COMPLEX*16
ALPHA, BETA
.TP 18
.ti +4
COMPLEX*16
AP( * ), X( * ), Y( * )
.SH PURPOSE
ZSPMV  performs the matrix-vector operation

where alpha and beta are scalars, x and y are n element vectors and
A is an n by n symmetric matrix, supplied in packed form.

.SH ARGUMENTS
.TP 9
UPLO     (input) CHARACTER*1
On entry, UPLO specifies whether the upper or lower
triangular part of the matrix A is supplied in the packed
array AP as follows:

UPLO = \(aqU\(aq or \(aqu\(aq   The upper triangular part of A is
supplied in AP.

UPLO = \(aqL\(aq or \(aql\(aq   The lower triangular part of A is
supplied in AP.

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
AP       (input) COMPLEX*16 array, dimension at least
( ( N*( N + 1 ) )/2 ).
Before entry, with UPLO = \(aqU\(aq or \(aqu\(aq, the array AP must
contain the upper triangular part of the symmetric matrix
packed sequentially, column by column, so that AP( 1 )
contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
and a( 2, 2 ) respectively, and so on.
Before entry, with UPLO = \(aqL\(aq or \(aql\(aq, the array AP must
contain the lower triangular part of the symmetric matrix
packed sequentially, column by column, so that AP( 1 )
contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
and a( 3, 1 ) respectively, and so on.
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
