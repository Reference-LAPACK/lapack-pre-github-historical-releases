.TH  SSPR 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
SSPR - the symmetric rank 1 operation   A := alpha*x*x\(aq + A,
.SH SYNOPSIS
.TP 40
SUBROUTINE SSPR(UPLO,N,ALPHA,X,INCX,AP)
.TP 40
.ti +4
REAL
ALPHA
.TP 40
.ti +4
INTEGER
INCX,N
.TP 40
.ti +4
CHARACTER
UPLO
.TP 40
.ti +4
REAL
AP(*),X(*)
.SH PURPOSE
SSPR    performs the symmetric rank 1 operation

where alpha is a real scalar, x is an n element vector and A is an
n by n symmetric matrix, supplied in packed form.
.br

.SH ARGUMENTS
.TP 7
UPLO   - CHARACTER*1.
On entry, UPLO specifies whether the upper or lower
triangular part of the matrix A is supplied in the packed
array AP as follows:

UPLO = \(aqU\(aq or \(aqu\(aq   The upper triangular part of A is
supplied in AP.

UPLO = \(aqL\(aq or \(aql\(aq   The lower triangular part of A is
supplied in AP.

Unchanged on exit.
.TP 7
N      - INTEGER.
On entry, N specifies the order of the matrix A.
N must be at least zero.
Unchanged on exit.
.TP 7
ALPHA  - REAL            .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
X      - REAL             array of dimension at least
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
AP     - REAL             array of DIMENSION at least
( ( n*( n + 1 ) )/2 ).
Before entry with  UPLO = \(aqU\(aq or \(aqu\(aq, the array AP must
contain the upper triangular part of the symmetric matrix
packed sequentially, column by column, so that AP( 1 )
contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
and a( 2, 2 ) respectively, and so on. On exit, the array
AP is overwritten by the upper triangular part of the
updated matrix.
Before entry with UPLO = \(aqL\(aq or \(aql\(aq, the array AP must
contain the lower triangular part of the symmetric matrix
packed sequentially, column by column, so that AP( 1 )
contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
and a( 3, 1 ) respectively, and so on. On exit, the array
AP is overwritten by the lower triangular part of the
updated matrix.

Level 2 Blas routine.

-- Written on 22-October-1986.
Jack Dongarra, Argonne National Lab.
Jeremy Du Croz, Nag Central Office.
Sven Hammarling, Nag Central Office.
Richard Hanson, Sandia National Labs.
