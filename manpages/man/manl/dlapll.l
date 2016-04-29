.TH  DLAPLL 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAPLL - column vectors X and Y, let   A = ( X Y )
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAPLL(
N, X, INCX, Y, INCY, SSMIN )
.TP 19
.ti +4
INTEGER
INCX, INCY, N
.TP 19
.ti +4
DOUBLE
PRECISION SSMIN
.TP 19
.ti +4
DOUBLE
PRECISION X( * ), Y( * )
.SH PURPOSE
Given two column vectors X and Y, let

The subroutine first computes the QR factorization of A = Q*R,
and then computes the SVD of the 2-by-2 upper triangular matrix R.
The smaller singular value of R is returned in SSMIN, which is used
as the measurement of the linear dependency of the vectors X and Y.

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The length of the vectors X and Y.
.TP 8
X       (input/output) DOUBLE PRECISION array,
dimension (1+(N-1)*INCX)
On entry, X contains the N-vector X.
On exit, X is overwritten.
.TP 8
INCX    (input) INTEGER
The increment between successive elements of X. INCX > 0.
.TP 8
Y       (input/output) DOUBLE PRECISION array,
dimension (1+(N-1)*INCY)
On entry, Y contains the N-vector Y.
On exit, Y is overwritten.
.TP 8
INCY    (input) INTEGER
The increment between successive elements of Y. INCY > 0.
.TP 8
SSMIN   (output) DOUBLE PRECISION
The smallest singular value of the N-by-2 matrix A = ( X Y ).
