.TH  ZTZRQF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTZRQF - i deprecated and has been replaced by routine ZTZRZF
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTZRQF(
M, N, A, LDA, TAU, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, M, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), TAU( * )
.SH PURPOSE
This routine is deprecated and has been replaced by routine ZTZRZF.

ZTZRQF reduces the M-by-N ( M<=N ) complex upper trapezoidal matrix A
to upper triangular form by means of unitary transformations.

The upper trapezoidal matrix A is factored as
.br

   A = ( R  0 ) * Z,
.br

where Z is an N-by-N unitary matrix and R is an M-by-M upper
triangular matrix.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= M.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the leading M-by-N upper trapezoidal part of the
array A must contain the matrix to be factorized.
On exit, the leading M-by-M upper triangular part of A
contains the upper triangular matrix R, and elements M+1 to
N of the first M rows of A, with the array TAU, represent the
unitary matrix Z as a product of M elementary reflectors.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
TAU     (output) COMPLEX*16 array, dimension (M)
The scalar factors of the elementary reflectors.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The  factorization is obtained by Householder\(aqs method.  The kth
transformation matrix, Z( k ), whose conjugate transpose is used to
introduce zeros into the (m - k + 1)th row of A, is given in the form

   Z( k ) = ( I     0   ),
.br
            ( 0  T( k ) )
.br

where
.br

   T( k ) = I - tau*u( k )*u( k )\(aq,   u( k ) = (   1    ),
                                               (   0    )
                                               ( z( k ) )

tau is a scalar and z( k ) is an ( n - m ) element vector.
tau and z( k ) are chosen to annihilate the elements of the kth row
of X.
.br

The scalar tau is returned in the kth element of TAU and the vector
u( k ) in the kth row of A, such that the elements of z( k ) are
in  a( k, m + 1 ), ..., a( k, n ). The elements of R are returned in
the upper triangular part of A.
.br

Z is given by
.br

   Z =  Z( 1 ) * Z( 2 ) * ... * Z( m ).
.br

