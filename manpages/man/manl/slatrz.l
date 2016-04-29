.TH  SLATRZ 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLATRZ - the M-by-(M+L) real upper trapezoidal matrix [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R 0 ) * Z, by means of orthogonal transformations
.SH SYNOPSIS
.TP 19
SUBROUTINE SLATRZ(
M, N, L, A, LDA, TAU, WORK )
.TP 19
.ti +4
INTEGER
L, LDA, M, N
.TP 19
.ti +4
REAL
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
SLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
[ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
matrix and, R and A1 are M-by-M upper triangular matrices.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
L       (input) INTEGER
The number of columns of the matrix A containing the
meaningful part of the Householder vectors. N-M >= L >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the leading M-by-N upper trapezoidal part of the
array A must contain the matrix to be factorized.
On exit, the leading M-by-M upper triangular part of A
contains the upper triangular matrix R, and elements N-L+1 to
N of the first M rows of A, with the array TAU, represent the
orthogonal matrix Z as a product of M elementary reflectors.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
TAU     (output) REAL array, dimension (M)
The scalar factors of the elementary reflectors.
.TP 8
WORK    (workspace) REAL array, dimension (M)
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

tau is a scalar and z( k ) is an l element vector. tau and z( k )
are chosen to annihilate the elements of the kth row of A2.

The scalar tau is returned in the kth element of TAU and the vector
u( k ) in the kth row of A2, such that the elements of z( k ) are
in  a( k, l + 1 ), ..., a( k, n ). The elements of R are returned in
the upper triangular part of A1.
.br

Z is given by
.br

   Z =  Z( 1 ) * Z( 2 ) * ... * Z( m ).
.br

