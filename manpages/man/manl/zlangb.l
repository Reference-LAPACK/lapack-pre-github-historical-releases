.TH  ZLANGB 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLANGB - the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of an n by n band matrix A, with kl sub-diagonals and ku super-diagonals
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION ZLANGB( NORM, N, KL, KU, AB, LDAB,
WORK )
.TP 17
.ti +4
CHARACTER
NORM
.TP 17
.ti +4
INTEGER
KL, KU, LDAB, N
.TP 17
.ti +4
DOUBLE
PRECISION WORK( * )
.TP 17
.ti +4
COMPLEX*16
AB( LDAB, * )
.SH PURPOSE
ZLANGB  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the element of  largest absolute value  of an
n by n band matrix  A,  with kl sub-diagonals and ku super-diagonals.

.SH DESCRIPTION
ZLANGB returns the value
.br

   ZLANGB = ( max(abs(A(i,j))), NORM = \(aqM\(aq or \(aqm\(aq
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
Specifies the value to be returned in ZLANGB as described
above.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, ZLANGB is
set to zero.
.TP 8
KL      (input) INTEGER
The number of sub-diagonals of the matrix A.  KL >= 0.
.TP 8
KU      (input) INTEGER
The number of super-diagonals of the matrix A.  KU >= 0.
.TP 8
AB      (input) COMPLEX*16 array, dimension (LDAB,N)
The band matrix A, stored in rows 1 to KL+KU+1.  The j-th
column of A is stored in the j-th column of the array AB as
follows:
AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(n,j+kl).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KL+KU+1.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
where LWORK >= N when NORM = \(aqI\(aq; otherwise, WORK is not
referenced.
