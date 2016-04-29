.TH  SLASSQ 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLASSQ - the values scl and smsq such that   ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASSQ(
N, X, INCX, SCALE, SUMSQ )
.TP 19
.ti +4
INTEGER
INCX, N
.TP 19
.ti +4
REAL
SCALE, SUMSQ
.TP 19
.ti +4
REAL
X( * )
.SH PURPOSE
SLASSQ  returns the values  scl  and  smsq  such that

where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
assumed to be non-negative and  scl  returns the value
.br

   scl = max( scale, abs( x( i ) ) ).
.br

scale and sumsq must be supplied in SCALE and SUMSQ and
.br
scl and smsq are overwritten on SCALE and SUMSQ respectively.

The routine makes only one pass through the vector x.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of elements to be used from the vector X.
.TP 8
X       (input) REAL array, dimension (N)
The vector for which a scaled sum of squares is computed.
x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
.TP 8
INCX    (input) INTEGER
The increment between successive values of the vector X.
INCX > 0.
.TP 8
SCALE   (input/output) REAL
On entry, the value  scale  in the equation above.
On exit, SCALE is overwritten with  scl , the scaling factor
for the sum of squares.
.TP 8
SUMSQ   (input/output) REAL
On entry, the value  sumsq  in the equation above.
On exit, SUMSQ is overwritten with  smsq , the basic sum of
squares from which  scl  has been factored out.
