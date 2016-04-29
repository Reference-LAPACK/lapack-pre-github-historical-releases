.TH  DSYMV 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
DSYMV - the matrix-vector operation   y := alpha*A*x + beta*y,
.SH SYNOPSIS
.TP 56
SUBROUTINE DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
.TP 56
.ti +4
DOUBLE
PRECISION ALPHA,BETA
.TP 56
.ti +4
INTEGER
INCX,INCY,LDA,N
.TP 56
.ti +4
CHARACTER
UPLO
.TP 56
.ti +4
DOUBLE
PRECISION A(LDA,*),X(*),Y(*)
.SH PURPOSE
DSYMV  performs the matrix-vector  operation

where alpha and beta are scalars, x and y are n element vectors and
A is an n by n symmetric matrix.
.br

.SH ARGUMENTS
.TP 7
UPLO   - CHARACTER*1.
On entry, UPLO specifies whether the upper or lower
triangular part of the array A is to be referenced as
follows:

UPLO = \(aqU\(aq or \(aqu\(aq   Only the upper triangular part of A
is to be referenced.

UPLO = \(aqL\(aq or \(aql\(aq   Only the lower triangular part of A
is to be referenced.

Unchanged on exit.
.TP 7
N      - INTEGER.
On entry, N specifies the order of the matrix A.
N must be at least zero.
Unchanged on exit.
.TP 7
ALPHA  - DOUBLE PRECISION.
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
Before entry with  UPLO = \(aqU\(aq or \(aqu\(aq, the leading n by n
upper triangular part of the array A must contain the upper
triangular part of the symmetric matrix and the strictly
lower triangular part of A is not referenced.
Before entry with UPLO = \(aqL\(aq or \(aql\(aq, the leading n by n
lower triangular part of the array A must contain the lower
triangular part of the symmetric matrix and the strictly
upper triangular part of A is not referenced.
Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. LDA must be at least
max( 1, n ).
Unchanged on exit.
.TP 7
X      - DOUBLE PRECISION array of dimension at least
( 1 + ( n - 1 )*abs( INCX ) ).
Before entry, the incremented array X must contain the n
element vector x.
Unchanged on exit.
.TP 7
INCX   - INTEGER.
On entry, INCX specifies the increment for the elements of
X. INCX must not be zero.
Unchanged on exit.
.TP 7
BETA   - DOUBLE PRECISION.
On entry, BETA specifies the scalar beta. When BETA is
supplied as zero then Y need not be set on input.
Unchanged on exit.
.TP 7
Y      - DOUBLE PRECISION array of dimension at least
( 1 + ( n - 1 )*abs( INCY ) ).
Before entry, the incremented array Y must contain the n
element vector y. On exit, Y is overwritten by the updated
vector y.
.TP 7
INCY   - INTEGER.
On entry, INCY specifies the increment for the elements of
Y. INCY must not be zero.
Unchanged on exit.

Level 2 Blas routine.

-- Written on 22-October-1986.
Jack Dongarra, Argonne National Lab.
Jeremy Du Croz, Nag Central Office.
Sven Hammarling, Nag Central Office.
Richard Hanson, Sandia National Labs.
