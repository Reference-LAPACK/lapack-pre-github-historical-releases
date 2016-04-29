.TH  DLANST 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLANST - the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a real symmetric tridiagonal matrix A
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DLANST( NORM, N, D, E )
.TP 17
.ti +4
CHARACTER
NORM
.TP 17
.ti +4
INTEGER
N
.TP 17
.ti +4
DOUBLE
PRECISION D( * ), E( * )
.SH PURPOSE
DLANST  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the  element of  largest absolute value  of a
real symmetric tridiagonal matrix A.

.SH DESCRIPTION
DLANST returns the value
.br

   DLANST = ( max(abs(A(i,j))), NORM = \(aqM\(aq or \(aqm\(aq
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
Specifies the value to be returned in DLANST as described
above.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, DLANST is
set to zero.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The diagonal elements of A.
.TP 8
E       (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) sub-diagonal or super-diagonal elements of A.
