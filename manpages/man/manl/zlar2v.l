.TH  ZLAR2V 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLAR2V - a vector of complex plane rotations with real cosines from both sides to a sequence of 2-by-2 complex Hermitian matrices,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAR2V(
N, X, Y, Z, INCX, C, S, INCC )
.TP 19
.ti +4
INTEGER
INCC, INCX, N
.TP 19
.ti +4
DOUBLE
PRECISION C( * )
.TP 19
.ti +4
COMPLEX*16
S( * ), X( * ), Y( * ), Z( * )
.SH PURPOSE
ZLAR2V applies a vector of complex plane rotations with real cosines
from both sides to a sequence of 2-by-2 complex Hermitian matrices,
defined by the elements of the vectors x, y and z. For i = 1,2,...,n

   (       x(i)  z(i) ) :=
.br
   ( conjg(z(i)) y(i) )
.br

     (  c(i) conjg(s(i)) ) (       x(i)  z(i) ) ( c(i) -conjg(s(i)) )
     ( -s(i)       c(i)  ) ( conjg(z(i)) y(i) ) ( s(i)        c(i)  )

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of plane rotations to be applied.
.TP 8
X       (input/output) COMPLEX*16 array, dimension (1+(N-1)*INCX)
The vector x; the elements of x are assumed to be real.
.TP 8
Y       (input/output) COMPLEX*16 array, dimension (1+(N-1)*INCX)
The vector y; the elements of y are assumed to be real.
.TP 8
Z       (input/output) COMPLEX*16 array, dimension (1+(N-1)*INCX)
The vector z.
.TP 8
INCX    (input) INTEGER
The increment between elements of X, Y and Z. INCX > 0.
.TP 8
C       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
The cosines of the plane rotations.
.TP 8
S       (input) COMPLEX*16 array, dimension (1+(N-1)*INCC)
The sines of the plane rotations.
.TP 8
INCC    (input) INTEGER
The increment between elements of C and S. INCC > 0.
