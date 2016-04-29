.TH  ZTBMV 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
ZTBMV - one of the matrix-vector operations   x := A*x, or x := A\(aq*x, or x := conjg( A\(aq )*x,
.SH SYNOPSIS
.TP 51
SUBROUTINE ZTBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
.TP 51
.ti +4
INTEGER
INCX,K,LDA,N
.TP 51
.ti +4
CHARACTER
DIAG,TRANS,UPLO
.TP 51
.ti +4
DOUBLE
COMPLEX A(LDA,*),X(*)
.SH PURPOSE
ZTBMV  performs one of the matrix-vector operations

where x is an n element vector and  A is an n by n unit, or non-unit,
upper or lower triangular band matrix, with ( k + 1 ) diagonals.

.SH ARGUMENTS
.TP 7
UPLO   - CHARACTER*1.
On entry, UPLO specifies whether the matrix is an upper or
lower triangular matrix as follows:

UPLO = \(aqU\(aq or \(aqu\(aq   A is an upper triangular matrix.

UPLO = \(aqL\(aq or \(aql\(aq   A is a lower triangular matrix.

Unchanged on exit.
.TP 7
TRANS  - CHARACTER*1.
On entry, TRANS specifies the operation to be performed as
follows:

TRANS = \(aqN\(aq or \(aqn\(aq   x := A*x.

TRANS = \(aqT\(aq or \(aqt\(aq   x := A\(aq*x.

TRANS = \(aqC\(aq or \(aqc\(aq   x := conjg( A\(aq )*x.

Unchanged on exit.
.TP 7
DIAG   - CHARACTER*1.
On entry, DIAG specifies whether or not A is unit
triangular as follows:

DIAG = \(aqU\(aq or \(aqu\(aq   A is assumed to be unit triangular.

DIAG = \(aqN\(aq or \(aqn\(aq   A is not assumed to be unit
triangular.

Unchanged on exit.
.TP 7
N      - INTEGER.
On entry, N specifies the order of the matrix A.
N must be at least zero.
Unchanged on exit.
.TP 7
K      - INTEGER.
On entry with UPLO = \(aqU\(aq or \(aqu\(aq, K specifies the number of
super-diagonals of the matrix A.
On entry with UPLO = \(aqL\(aq or \(aql\(aq, K specifies the number of
sub-diagonals of the matrix A.
K must satisfy  0 .le. K.
Unchanged on exit.
.TP 7
A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
Before entry with UPLO = \(aqU\(aq or \(aqu\(aq, the leading ( k + 1 )
by n part of the array A must contain the upper triangular
band part of the matrix of coefficients, supplied column by
column, with the leading diagonal of the matrix in row
( k + 1 ) of the array, the first super-diagonal starting at
position 2 in row k, and so on. The top left k by k triangle
of the array A is not referenced.
The following program segment will transfer an upper
triangular band matrix from conventional full matrix storage
to band storage:

DO 20, J = 1, N
M = K + 1 - J
DO 10, I = MAX( 1, J - K ), J
A( M + I, J ) = matrix( I, J )
10    CONTINUE
20 CONTINUE

Before entry with UPLO = \(aqL\(aq or \(aql\(aq, the leading ( k + 1 )
by n part of the array A must contain the lower triangular
band part of the matrix of coefficients, supplied column by
column, with the leading diagonal of the matrix in row 1 of
the array, the first sub-diagonal starting at position 1 in
row 2, and so on. The bottom right k by k triangle of the
array A is not referenced.
The following program segment will transfer a lower
triangular band matrix from conventional full matrix storage
to band storage:

DO 20, J = 1, N
M = 1 - J
DO 10, I = J, MIN( N, J + K )
A( M + I, J ) = matrix( I, J )
10    CONTINUE
20 CONTINUE

Note that when DIAG = \(aqU\(aq or \(aqu\(aq the elements of the array A
corresponding to the diagonal elements of the matrix are not
referenced, but are assumed to be unity.
Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. LDA must be at least
( k + 1 ).
Unchanged on exit.
.TP 7
X      - COMPLEX*16       array of dimension at least
( 1 + ( n - 1 )*abs( INCX ) ).
Before entry, the incremented array X must contain the n
element vector x. On exit, X is overwritten with the
tranformed vector x.
.TP 7
INCX   - INTEGER.
On entry, INCX specifies the increment for the elements of
X. INCX must not be zero.
Unchanged on exit.

Level 2 Blas routine.

-- Written on 22-October-1986.
Jack Dongarra, Argonne National Lab.
Jeremy Du Croz, Nag Central Office.
Sven Hammarling, Nag Central Office.
Richard Hanson, Sandia National Labs.
