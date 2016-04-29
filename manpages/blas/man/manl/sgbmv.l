.TH  SGBMV 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
SGBMV - one of the matrix-vector operations   y := alpha*A*x + beta*y, or y := alpha*A\(aq*x + beta*y,
.SH SYNOPSIS
.TP 65
SUBROUTINE SGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
.TP 65
.ti +4
REAL
ALPHA,BETA
.TP 65
.ti +4
INTEGER
INCX,INCY,KL,KU,LDA,M,N
.TP 65
.ti +4
CHARACTER
TRANS
.TP 65
.ti +4
REAL
A(LDA,*),X(*),Y(*)
.SH PURPOSE
SGBMV  performs one of the matrix-vector operations

where alpha and beta are scalars, x and y are vectors and A is an
m by n band matrix, with kl sub-diagonals and ku super-diagonals.

.SH ARGUMENTS
.TP 7
TRANS  - CHARACTER*1.
On entry, TRANS specifies the operation to be performed as
follows:

TRANS = \(aqN\(aq or \(aqn\(aq   y := alpha*A*x + beta*y.

TRANS = \(aqT\(aq or \(aqt\(aq   y := alpha*A\(aq*x + beta*y.

TRANS = \(aqC\(aq or \(aqc\(aq   y := alpha*A\(aq*x + beta*y.

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
KL     - INTEGER.
On entry, KL specifies the number of sub-diagonals of the
matrix A. KL must satisfy  0 .le. KL.
Unchanged on exit.
.TP 7
KU     - INTEGER.
On entry, KU specifies the number of super-diagonals of the
matrix A. KU must satisfy  0 .le. KU.
Unchanged on exit.
.TP 7
ALPHA  - REAL            .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - REAL             array of DIMENSION ( LDA, n ).
Before entry, the leading ( kl + ku + 1 ) by n part of the
array A must contain the matrix of coefficients, supplied
column by column, with the leading diagonal of the matrix in
row ( ku + 1 ) of the array, the first super-diagonal
starting at position 2 in row ku, the first sub-diagonal
starting at position 1 in row ( ku + 2 ), and so on.
Elements in the array A that do not correspond to elements
in the band matrix (such as the top left ku by ku triangle)
are not referenced.
The following program segment will transfer a band matrix
from conventional full matrix storage to band storage:

DO 20, J = 1, N
K = KU + 1 - J
DO 10, I = MAX( 1, J - KU ), MIN( M, J + KL )
A( K + I, J ) = matrix( I, J )
10    CONTINUE
20 CONTINUE

Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. LDA must be at least
( kl + ku + 1 ).
Unchanged on exit.
.TP 7
X      - REAL             array of DIMENSION at least
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
BETA   - REAL            .
On entry, BETA specifies the scalar beta. When BETA is
supplied as zero then Y need not be set on input.
Unchanged on exit.
.TP 7
Y      - REAL             array of DIMENSION at least
( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = \(aqN\(aq or \(aqn\(aq
and at least
( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
Before entry, the incremented array Y must contain the
vector y. On exit, Y is overwritten by the updated vector y.
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
