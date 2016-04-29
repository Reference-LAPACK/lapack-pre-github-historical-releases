.TH  DRSCL 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DRSCL - an n-element real vector x by the real scalar 1/a
.SH SYNOPSIS
.TP 18
SUBROUTINE DRSCL(
N, SA, SX, INCX )
.TP 18
.ti +4
INTEGER
INCX, N
.TP 18
.ti +4
DOUBLE
PRECISION SA
.TP 18
.ti +4
DOUBLE
PRECISION SX( * )
.SH PURPOSE
DRSCL multiplies an n-element real vector x by the real scalar 1/a.
This is done without overflow or underflow as long as
.br
the final result x/a does not overflow or underflow.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of components of the vector x.
.TP 8
SA      (input) DOUBLE PRECISION
The scalar a which is used to divide each component of x.
SA must be >= 0, or the subroutine will divide by zero.
.TP 8
SX      (input/output) DOUBLE PRECISION array, dimension
(1+(N-1)*abs(INCX))
The n-element vector x.
.TP 8
INCX    (input) INTEGER
The increment between successive values of the vector SX.
> 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n
