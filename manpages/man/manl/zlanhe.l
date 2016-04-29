.TH  ZLANHE 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLANHE - the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a complex hermitian matrix A
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION ZLANHE( NORM, UPLO, N, A, LDA, WORK )
.TP 17
.ti +4
CHARACTER
NORM, UPLO
.TP 17
.ti +4
INTEGER
LDA, N
.TP 17
.ti +4
DOUBLE
PRECISION WORK( * )
.TP 17
.ti +4
COMPLEX*16
A( LDA, * )
.SH PURPOSE
ZLANHE  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the  element of  largest absolute value  of a
complex hermitian matrix A.

.SH DESCRIPTION
ZLANHE returns the value
.br

   ZLANHE = ( max(abs(A(i,j))), NORM = \(aqM\(aq or \(aqm\(aq
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
Specifies the value to be returned in ZLANHE as described
above.
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
hermitian matrix A is to be referenced.
= \(aqU\(aq:  Upper triangular part of A is referenced
.br
= \(aqL\(aq:  Lower triangular part of A is referenced
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, ZLANHE is
set to zero.
.TP 8
A       (input) COMPLEX*16 array, dimension (LDA,N)
The hermitian matrix A.  If UPLO = \(aqU\(aq, the leading n by n
upper triangular part of A contains the upper triangular part
of the matrix A, and the strictly lower triangular part of A
is not referenced.  If UPLO = \(aqL\(aq, the leading n by n lower
triangular part of A contains the lower triangular part of
the matrix A, and the strictly upper triangular part of A is
not referenced. Note that the imaginary parts of the diagonal
elements need not be set and are assumed to be zero.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(N,1).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
where LWORK >= N when NORM = \(aqI\(aq or \(aq1\(aq or \(aqO\(aq; otherwise,
WORK is not referenced.
