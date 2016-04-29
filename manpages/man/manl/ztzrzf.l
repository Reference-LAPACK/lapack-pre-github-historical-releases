.TH  ZTZRZF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTZRZF - the M-by-N ( M<=N ) complex upper trapezoidal matrix A to upper triangular form by means of unitary transformations
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTZRZF(
M, N, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, M, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
ZTZRZF reduces the M-by-N ( M<=N ) complex upper trapezoidal matrix A
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
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,M).
For optimum performance LWORK >= M*NB, where NB is
the optimal blocksize.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
Based on contributions by
.br
  A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA

The factorization is obtained by Householder\(aqs method.  The kth
transformation matrix, Z( k ), which is used to introduce zeros into
the ( m - k + 1 )th row of A, is given in the form
.br

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

