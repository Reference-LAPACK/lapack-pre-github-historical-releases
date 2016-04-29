.TH  ZGEMV 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
ZGEMV - one of the matrix-vector operations   y := alpha*A*x + beta*y, or y := alpha*A\(aq*x + beta*y, or   y := alpha*conjg( A\(aq )*x + beta*y,
.SH SYNOPSIS
.TP 59
SUBROUTINE ZGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
.TP 59
.ti +4
DOUBLE
COMPLEX ALPHA,BETA
.TP 59
.ti +4
INTEGER
INCX,INCY,LDA,M,N
.TP 59
.ti +4
CHARACTER
TRANS
.TP 59
.ti +4
DOUBLE
COMPLEX A(LDA,*),X(*),Y(*)
.SH PURPOSE
ZGEMV  performs one of the matrix-vector operations

where alpha and beta are scalars, x and y are vectors and A is an
m by n matrix.
.br

.SH ARGUMENTS
.TP 7
TRANS  - CHARACTER*1.
On entry, TRANS specifies the operation to be performed as
follows:

TRANS = \(aqN\(aq or \(aqn\(aq   y := alpha*A*x + beta*y.

TRANS = \(aqT\(aq or \(aqt\(aq   y := alpha*A\(aq*x + beta*y.

TRANS = \(aqC\(aq or \(aqc\(aq   y := alpha*conjg( A\(aq )*x + beta*y.

Unchanged on exit.
.TP 7
M      - INTEGER.
On entry, M specifies the number of rows of the matrix A.
M must be at least zero.
Unchanged on exit.
.TP 7
N      - INTEGER.
On entry, N specifies the number of columns of the matrix A.
N must be at least zero.
Unchanged on exit.
.TP 7
ALPHA  - COMPLEX*16      .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
Before entry, the leading m by n part of the array A must
contain the matrix of coefficients.
Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. LDA must be at least
max( 1, m ).
Unchanged on exit.
.TP 7
X      - COMPLEX*16       array of DIMENSION at least
( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = \(aqN\(aq or \(aqn\(aq
and at least
( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
Before entry, the incremented array X must contain the
vector x.
Unchanged on exit.
.TP 7
INCX   - INTEGER.
On entry, INCX specifies the increment for the elements of
X. INCX must not be zero.
Unchanged on exit.
.TP 7
BETA   - COMPLEX*16      .
On entry, BETA specifies the scalar beta. When BETA is
supplied as zero then Y need not be set on input.
Unchanged on exit.
.TP 7
Y      - COMPLEX*16       array of DIMENSION at least
( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = \(aqN\(aq or \(aqn\(aq
and at least
( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
Before entry with BETA non-zero, the incremented array Y
must contain the vector y. On exit, Y is overwritten by the
updated vector y.
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
