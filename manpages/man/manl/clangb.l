.TH  CLANGB 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLANGB - the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of an n by n band matrix A, with kl sub-diagonals and ku super-diagonals
.SH SYNOPSIS
.TP 14
REAL FUNCTION
CLANGB( NORM, N, KL, KU, AB, LDAB,
WORK )
.TP 14
.ti +4
CHARACTER
NORM
.TP 14
.ti +4
INTEGER
KL, KU, LDAB, N
.TP 14
.ti +4
REAL
WORK( * )
.TP 14
.ti +4
COMPLEX
AB( LDAB, * )
.SH PURPOSE
CLANGB  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the element of  largest absolute value  of an
n by n band matrix  A,  with kl sub-diagonals and ku super-diagonals.

.SH DESCRIPTION
CLANGB returns the value
.br

   CLANGB = ( max(abs(A(i,j))), NORM = \(aqM\(aq or \(aqm\(aq
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
Specifies the value to be returned in CLANGB as described
above.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, CLANGB is
set to zero.
.TP 8
KL      (input) INTEGER
The number of sub-diagonals of the matrix A.  KL >= 0.
.TP 8
KU      (input) INTEGER
The number of super-diagonals of the matrix A.  KU >= 0.
.TP 8
AB      (input) COMPLEX array, dimension (LDAB,N)
The band matrix A, stored in rows 1 to KL+KU+1.  The j-th
column of A is stored in the j-th column of the array AB as
follows:
AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(n,j+kl).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KL+KU+1.
.TP 8
WORK    (workspace) REAL array, dimension (MAX(1,LWORK)),
where LWORK >= N when NORM = \(aqI\(aq; otherwise, WORK is not
referenced.
