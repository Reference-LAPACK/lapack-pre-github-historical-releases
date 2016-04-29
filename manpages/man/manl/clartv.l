.TH  CLARTV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLARTV - a vector of complex plane rotations with real cosines to elements of the complex vectors x and y
.SH SYNOPSIS
.TP 19
SUBROUTINE CLARTV(
N, X, INCX, Y, INCY, C, S, INCC )
.TP 19
.ti +4
INTEGER
INCC, INCX, INCY, N
.TP 19
.ti +4
REAL
C( * )
.TP 19
.ti +4
COMPLEX
S( * ), X( * ), Y( * )
.SH PURPOSE
CLARTV applies a vector of complex plane rotations with real cosines
to elements of the complex vectors x and y. For i = 1,2,...,n

   ( x(i) ) := (        c(i)   s(i) ) ( x(i) )
.br
   ( y(i) )    ( -conjg(s(i))  c(i) ) ( y(i) )
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of plane rotations to be applied.
.TP 8
X       (input/output) COMPLEX array, dimension (1+(N-1)*INCX)
The vector x.
.TP 8
INCX    (input) INTEGER
The increment between elements of X. INCX > 0.
.TP 8
Y       (input/output) COMPLEX array, dimension (1+(N-1)*INCY)
The vector y.
.TP 8
INCY    (input) INTEGER
The increment between elements of Y. INCY > 0.
.TP 8
C       (input) REAL array, dimension (1+(N-1)*INCC)
The cosines of the plane rotations.
.TP 8
S       (input) COMPLEX array, dimension (1+(N-1)*INCC)
The sines of the plane rotations.
.TP 8
INCC    (input) INTEGER
The increment between elements of C and S. INCC > 0.
