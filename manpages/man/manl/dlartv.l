.TH  DLARTV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARTV - a vector of real plane rotations to elements of the real vectors x and y
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARTV(
N, X, INCX, Y, INCY, C, S, INCC )
.TP 19
.ti +4
INTEGER
INCC, INCX, INCY, N
.TP 19
.ti +4
DOUBLE
PRECISION C( * ), S( * ), X( * ), Y( * )
.SH PURPOSE
DLARTV applies a vector of real plane rotations to elements of the
real vectors x and y. For i = 1,2,...,n

   ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
.br
   ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of plane rotations to be applied.
.TP 8
X       (input/output) DOUBLE PRECISION array,
dimension (1+(N-1)*INCX)
The vector x.
.TP 8
INCX    (input) INTEGER
The increment between elements of X. INCX > 0.
.TP 8
Y       (input/output) DOUBLE PRECISION array,
dimension (1+(N-1)*INCY)
The vector y.
.TP 8
INCY    (input) INTEGER
The increment between elements of Y. INCY > 0.
.TP 8
C       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
The cosines of the plane rotations.
.TP 8
S       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
The sines of the plane rotations.
.TP 8
INCC    (input) INTEGER
The increment between elements of C and S. INCC > 0.
