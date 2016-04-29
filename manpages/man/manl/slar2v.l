.TH  SLAR2V 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAR2V - a vector of real plane rotations from both sides to a sequence of 2-by-2 real symmetric matrices, defined by the elements of the vectors x, y and z
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAR2V(
N, X, Y, Z, INCX, C, S, INCC )
.TP 19
.ti +4
INTEGER
INCC, INCX, N
.TP 19
.ti +4
REAL
C( * ), S( * ), X( * ), Y( * ), Z( * )
.SH PURPOSE
SLAR2V applies a vector of real plane rotations from both sides to
a sequence of 2-by-2 real symmetric matrices, defined by the elements
of the vectors x, y and z. For i = 1,2,...,n

   ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
   ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of plane rotations to be applied.
.TP 8
X       (input/output) REAL array,
dimension (1+(N-1)*INCX)
The vector x.
.TP 8
Y       (input/output) REAL array,
dimension (1+(N-1)*INCX)
The vector y.
.TP 8
Z       (input/output) REAL array,
dimension (1+(N-1)*INCX)
The vector z.
.TP 8
INCX    (input) INTEGER
The increment between elements of X, Y and Z. INCX > 0.
.TP 8
C       (input) REAL array, dimension (1+(N-1)*INCC)
The cosines of the plane rotations.
.TP 8
S       (input) REAL array, dimension (1+(N-1)*INCC)
The sines of the plane rotations.
.TP 8
INCC    (input) INTEGER
The increment between elements of C and S. INCC > 0.
