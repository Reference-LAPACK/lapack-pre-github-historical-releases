.TH  CSRSCL 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CSRSCL - an n-element complex vector x by the real scalar 1/a
.SH SYNOPSIS
.TP 19
SUBROUTINE CSRSCL(
N, SA, SX, INCX )
.TP 19
.ti +4
INTEGER
INCX, N
.TP 19
.ti +4
REAL
SA
.TP 19
.ti +4
COMPLEX
SX( * )
.SH PURPOSE
CSRSCL multiplies an n-element complex vector x by the real scalar
1/a.  This is done without overflow or underflow as long as
the final result x/a does not overflow or underflow.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of components of the vector x.
.TP 8
SA      (input) REAL
The scalar a which is used to divide each component of x.
SA must be >= 0, or the subroutine will divide by zero.
.TP 8
SX      (input/output) COMPLEX array, dimension
(1+(N-1)*abs(INCX))
The n-element vector x.
.TP 8
INCX    (input) INTEGER
The increment between successive values of the vector SX.
> 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n
