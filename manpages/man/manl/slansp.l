.TH  SLANSP 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLANSP - the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a real symmetric matrix A, supplied in packed form
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SLANSP( NORM, UPLO, N, AP, WORK )
.TP 14
.ti +4
CHARACTER
NORM, UPLO
.TP 14
.ti +4
INTEGER
N
.TP 14
.ti +4
REAL
AP( * ), WORK( * )
.SH PURPOSE
SLANSP  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the  element of  largest absolute value  of a
real symmetric matrix A,  supplied in packed form.

.SH DESCRIPTION
SLANSP returns the value
.br

   SLANSP = ( max(abs(A(i,j))), NORM = \(aqM\(aq or \(aqm\(aq
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
Specifies the value to be returned in SLANSP as described
above.
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is supplied.
= \(aqU\(aq:  Upper triangular part of A is supplied
.br
= \(aqL\(aq:  Lower triangular part of A is supplied
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, SLANSP is
set to zero.
.TP 8
AP      (input) REAL array, dimension (N*(N+1)/2)
The upper or lower triangle of the symmetric matrix A, packed
columnwise in a linear array.  The j-th column of A is stored
in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
.TP 8
WORK    (workspace) REAL array, dimension (MAX(1,LWORK)),
where LWORK >= N when NORM = \(aqI\(aq or \(aq1\(aq or \(aqO\(aq; otherwise,
WORK is not referenced.
