.TH  ZLASSQ 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLASSQ - the values scl and ssq such that   ( scl**2 )*ssq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLASSQ(
N, X, INCX, SCALE, SUMSQ )
.TP 19
.ti +4
INTEGER
INCX, N
.TP 19
.ti +4
DOUBLE
PRECISION SCALE, SUMSQ
.TP 19
.ti +4
COMPLEX*16
X( * )
.SH PURPOSE
ZLASSQ returns the values scl and ssq such that

where x( i ) = abs( X( 1 + ( i - 1 )*INCX ) ). The value of sumsq is
assumed to be at least unity and the value of ssq will then satisfy

   1.0 .le. ssq .le. ( sumsq + 2*n ).
.br

scale is assumed to be non-negative and scl returns the value

   scl = max( scale, abs( real( x( i ) ) ), abs( aimag( x( i ) ) ) ),
          i
.br

scale and sumsq must be supplied in SCALE and SUMSQ respectively.
SCALE and SUMSQ are overwritten by scl and ssq respectively.

The routine makes only one pass through the vector X.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of elements to be used from the vector X.
.TP 8
X       (input) COMPLEX*16 array, dimension (N)
The vector x as described above.
x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
.TP 8
INCX    (input) INTEGER
The increment between successive values of the vector X.
INCX > 0.
.TP 8
SCALE   (input/output) DOUBLE PRECISION
On entry, the value  scale  in the equation above.
On exit, SCALE is overwritten with the value  scl .
.TP 8
SUMSQ   (input/output) DOUBLE PRECISION
On entry, the value  sumsq  in the equation above.
On exit, SUMSQ is overwritten with the value  ssq .
