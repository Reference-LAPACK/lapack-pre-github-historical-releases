.TH  SLANTB 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLANTB - the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of an n by n triangular band matrix A, with ( k + 1 ) diagonals
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SLANTB( NORM, UPLO, DIAG, N, K, AB,
LDAB, WORK )
.TP 14
.ti +4
CHARACTER
DIAG, NORM, UPLO
.TP 14
.ti +4
INTEGER
K, LDAB, N
.TP 14
.ti +4
REAL
AB( LDAB, * ), WORK( * )
.SH PURPOSE
SLANTB  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the element of  largest absolute value  of an
n by n triangular band matrix A,  with ( k + 1 ) diagonals.

.SH DESCRIPTION
SLANTB returns the value
.br

   SLANTB = ( max(abs(A(i,j))), NORM = \(aqM\(aq or \(aqm\(aq
.br
            (
.br
            ( norm1(A),         NORM = \(aq1\(aq, \(aqO\(aq or \(aqo\(aq
.br
            (
.br
            ( normI(A),         NORM = \(aqI\(aq or \(aqi\(aq
.br
            (
.br
            ( normF(A),         NORM = \(aqF\(aq, \(aqf\(aq, \(aqE\(aq or \(aqe\(aq

where  norm1  denotes the  one norm of a matrix (maximum column sum),
normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
normF  denotes the  Frobenius norm of a matrix (square root of sum of
squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.

.SH ARGUMENTS
.TP 8
NORM    (input) CHARACTER*1
Specifies the value to be returned in SLANTB as described
above.
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the matrix A is upper or lower triangular.
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
DIAG    (input) CHARACTER*1
Specifies whether or not the matrix A is unit triangular.
= \(aqN\(aq:  Non-unit triangular
.br
= \(aqU\(aq:  Unit triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, SLANTB is
set to zero.
.TP 8
K       (input) INTEGER
The number of super-diagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of sub-diagonals of the matrix A if UPLO = \(aqL\(aq.
K >= 0.
.TP 8
AB      (input) REAL array, dimension (LDAB,N)
The upper or lower triangular band matrix A, stored in the
first k+1 rows of AB.  The j-th column of A is stored
in the j-th column of the array AB as follows:
if UPLO = \(aqU\(aq, AB(k+1+i-j,j) = A(i,j) for max(1,j-k)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)   = A(i,j) for j<=i<=min(n,j+k).
Note that when DIAG = \(aqU\(aq, the elements of the array AB
corresponding to the diagonal elements of the matrix A are
not referenced, but are assumed to be one.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= K+1.
.TP 8
WORK    (workspace) REAL array, dimension (MAX(1,LWORK)),
where LWORK >= N when NORM = \(aqI\(aq; otherwise, WORK is not
referenced.
