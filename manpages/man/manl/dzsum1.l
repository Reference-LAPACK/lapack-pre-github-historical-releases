.TH  DZSUM1 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DZSUM1 - the sum of the absolute values of a complex vector and returns a double precision result
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DZSUM1( N, CX, INCX )
.TP 17
.ti +4
INTEGER
INCX, N
.TP 17
.ti +4
COMPLEX*16
CX( * )
.SH PURPOSE
DZSUM1 takes the sum of the absolute values of a complex
vector and returns a double precision result.

Based on DZASUM from the Level 1 BLAS.
.br
The change is to use the \(aqgenuine\(aq absolute value.
.br

Contributed by Nick Higham for use with ZLACON.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of elements in the vector CX.
.TP 8
CX      (input) COMPLEX*16 array, dimension (N)
The vector whose elements will be summed.
.TP 8
INCX    (input) INTEGER
The spacing between successive values of CX.  INCX > 0.
