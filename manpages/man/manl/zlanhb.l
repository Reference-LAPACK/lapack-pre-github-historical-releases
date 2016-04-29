.TH  ZLANHB 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLANHB - the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of an n by n hermitian band matrix A, with k super-diagonals
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION ZLANHB( NORM, UPLO, N, K, AB, LDAB,
WORK )
.TP 17
.ti +4
CHARACTER
NORM, UPLO
.TP 17
.ti +4
INTEGER
K, LDAB, N
.TP 17
.ti +4
DOUBLE
PRECISION WORK( * )
.TP 17
.ti +4
COMPLEX*16
AB( LDAB, * )
.SH PURPOSE
ZLANHB  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the element of  largest absolute value  of an
n by n hermitian band matrix A,  with k super-diagonals.

.SH DESCRIPTION
ZLANHB returns the value
.br

   ZLANHB = ( max(abs(A(i,j))), NORM = \(aqM\(aq or \(aqm\(aq
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
Specifies the value to be returned in ZLANHB as described
above.
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
band matrix A is supplied.
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, ZLANHB is
set to zero.
.TP 8
K       (input) INTEGER
The number of super-diagonals or sub-diagonals of the
band matrix A.  K >= 0.
.TP 8
AB      (input) COMPLEX*16 array, dimension (LDAB,N)
The upper or lower triangle of the hermitian band matrix A,
stored in the first K+1 rows of AB.  The j-th column of A is
stored in the j-th column of the array AB as follows:
if UPLO = \(aqU\(aq, AB(k+1+i-j,j) = A(i,j) for max(1,j-k)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)   = A(i,j) for j<=i<=min(n,j+k).
Note that the imaginary parts of the diagonal elements need
not be set and are assumed to be zero.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= K+1.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
where LWORK >= N when NORM = \(aqI\(aq or \(aq1\(aq or \(aqO\(aq; otherwise,
WORK is not referenced.
