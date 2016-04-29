.TH  CGERC 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CGERC - the rank 1 operation   A := alpha*x*conjg( y\(aq ) + A,
.SH SYNOPSIS
.TP 48
SUBROUTINE CGERC(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
.TP 48
.ti +4
COMPLEX
ALPHA
.TP 48
.ti +4
INTEGER
INCX,INCY,LDA,M,N
.TP 48
.ti +4
COMPLEX
A(LDA,*),X(*),Y(*)
.SH PURPOSE
CGERC  performs the rank 1 operation

where alpha is a scalar, x is an m element vector, y is an n element
vector and A is an m by n matrix.
.br

.SH ARGUMENTS
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
ALPHA  - COMPLEX         .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
X      - COMPLEX          array of dimension at least
( 1 + ( m - 1 )*abs( INCX ) ).
Before entry, the incremented array X must contain the m
element vector x.
Unchanged on exit.
.TP 7
INCX   - INTEGER.
On entry, INCX specifies the increment for the elements of
X. INCX must not be zero.
Unchanged on exit.
.TP 7
Y      - COMPLEX          array of dimension at least
( 1 + ( n - 1 )*abs( INCY ) ).
Before entry, the incremented array Y must contain the n
element vector y.
Unchanged on exit.
.TP 7
INCY   - INTEGER.
On entry, INCY specifies the increment for the elements of
Y. INCY must not be zero.
Unchanged on exit.
.TP 7
A      - COMPLEX          array of DIMENSION ( LDA, n ).
Before entry, the leading m by n part of the array A must
contain the matrix of coefficients. On exit, A is
overwritten by the updated matrix.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. LDA must be at least
max( 1, m ).
Unchanged on exit.

Level 2 Blas routine.

-- Written on 22-October-1986.
Jack Dongarra, Argonne National Lab.
Jeremy Du Croz, Nag Central Office.
Sven Hammarling, Nag Central Office.
Richard Hanson, Sandia National Labs.
