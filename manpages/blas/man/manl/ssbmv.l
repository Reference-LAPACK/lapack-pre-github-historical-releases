.TH  SSBMV 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
SSBMV - the matrix-vector operation   y := alpha*A*x + beta*y,
.SH SYNOPSIS
.TP 58
SUBROUTINE SSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
.TP 58
.ti +4
REAL
ALPHA,BETA
.TP 58
.ti +4
INTEGER
INCX,INCY,K,LDA,N
.TP 58
.ti +4
CHARACTER
UPLO
.TP 58
.ti +4
REAL
A(LDA,*),X(*),Y(*)
.SH PURPOSE
SSBMV  performs the matrix-vector  operation

where alpha and beta are scalars, x and y are n element vectors and
A is an n by n symmetric band matrix, with k super-diagonals.

.SH ARGUMENTS
.TP 7
UPLO   - CHARACTER*1.
On entry, UPLO specifies whether the upper or lower
triangular part of the band matrix A is being supplied as
follows:

UPLO = \(aqU\(aq or \(aqu\(aq   The upper triangular part of A is
being supplied.

UPLO = \(aqL\(aq or \(aql\(aq   The lower triangular part of A is
being supplied.

Unchanged on exit.
.TP 7
N      - INTEGER.
On entry, N specifies the order of the matrix A.
N must be at least zero.
Unchanged on exit.
.TP 7
K      - INTEGER.
On entry, K specifies the number of super-diagonals of the
matrix A. K must satisfy  0 .le. K.
Unchanged on exit.
.TP 7
ALPHA  - REAL            .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - REAL             array of DIMENSION ( LDA, n ).
Before entry with UPLO = \(aqU\(aq or \(aqu\(aq, the leading ( k + 1 )
by n part of the array A must contain the upper triangular
band part of the symmetric matrix, supplied column by
column, with the leading diagonal of the matrix in row
( k + 1 ) of the array, the first super-diagonal starting at
position 2 in row k, and so on. The top left k by k triangle
of the array A is not referenced.
The following program segment will transfer the upper
triangular part of a symmetric band matrix from conventional
full matrix storage to band storage:

DO 20, J = 1, N
M = K + 1 - J
DO 10, I = MAX( 1, J - K ), J
A( M + I, J ) = matrix( I, J )
10    CONTINUE
20 CONTINUE

Before entry with UPLO = \(aqL\(aq or \(aql\(aq, the leading ( k + 1 )
by n part of the array A must contain the lower triangular
band part of the symmetric matrix, supplied column by
column, with the leading diagonal of the matrix in row 1 of
the array, the first sub-diagonal starting at position 1 in
row 2, and so on. The bottom right k by k triangle of the
array A is not referenced.
The following program segment will transfer the lower
triangular part of a symmetric band matrix from conventional
full matrix storage to band storage:

DO 20, J = 1, N
M = 1 - J
DO 10, I = J, MIN( N, J + K )
A( M + I, J ) = matrix( I, J )
10    CONTINUE
20 CONTINUE

Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. LDA must be at least
( k + 1 ).
Unchanged on exit.
.TP 7
X      - REAL             array of DIMENSION at least
( 1 + ( n - 1 )*abs( INCX ) ).
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
On entry, BETA specifies the scalar beta.
Unchanged on exit.
.TP 7
Y      - REAL             array of DIMENSION at least
( 1 + ( n - 1 )*abs( INCY ) ).
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
