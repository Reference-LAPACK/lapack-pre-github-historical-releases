.TH  CSYR 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CSYR - the symmetric rank 1 operation   A := alpha*x*( x\(aq ) + A,
.SH SYNOPSIS
.TP 17
SUBROUTINE CSYR(
UPLO, N, ALPHA, X, INCX, A, LDA )
.TP 17
.ti +4
CHARACTER
UPLO
.TP 17
.ti +4
INTEGER
INCX, LDA, N
.TP 17
.ti +4
COMPLEX
ALPHA
.TP 17
.ti +4
COMPLEX
A( LDA, * ), X( * )
.SH PURPOSE
CSYR   performs the symmetric rank 1 operation

where alpha is a complex scalar, x is an n element vector and A is an
n by n symmetric matrix.
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
ALPHA    (input) COMPLEX
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 9
X        (input) COMPLEX array, dimension at least
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
A        (input/output) COMPLEX array, dimension ( LDA, N )
Before entry, with  UPLO = \(aqU\(aq or \(aqu\(aq, the leading n by n
upper triangular part of the array A must contain the upper
triangular part of the symmetric matrix and the strictly
lower triangular part of A is not referenced. On exit, the
upper triangular part of the array A is overwritten by the
upper triangular part of the updated matrix.
Before entry, with UPLO = \(aqL\(aq or \(aql\(aq, the leading n by n
lower triangular part of the array A must contain the lower
triangular part of the symmetric matrix and the strictly
upper triangular part of A is not referenced. On exit, the
lower triangular part of the array A is overwritten by the
lower triangular part of the updated matrix.
.TP 9
LDA      (input) INTEGER
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. LDA must be at least
max( 1, N ).
Unchanged on exit.
