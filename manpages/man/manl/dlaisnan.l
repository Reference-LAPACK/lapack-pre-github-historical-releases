.TH  DLAISNAN 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAISNAN - i not for general use
.SH SYNOPSIS
.TP 17
LOGICAL FUNCTION
DLAISNAN(DIN1,DIN2)
.TP 17
.ti +4
DOUBLE
PRECISION DIN1,DIN2
.SH PURPOSE
This routine is not for general use.  It exists solely to avoid
over-optimization in DISNAN.
.br

DLAISNAN checks for NaNs by comparing its two arguments for
inequality.  NaN is the only floating-point value where NaN != NaN
returns .TRUE.  To check for NaNs, pass the same variable as both
arguments.
.br

Strictly speaking, Fortran does not allow aliasing of function
arguments. So a compiler must assume that the two arguments are
not the same variable, and the test will not be optimized away.
Interprocedural or whole-program optimization may delete this
test.  The ISNAN functions will be replaced by the correct
Fortran 03 intrinsic once the intrinsic is widely available.

.SH ARGUMENTS
.TP 9
DIN1     (input) DOUBLE PRECISION
DIN2     (input) DOUBLE PRECISION
Two numbers to compare for inequality.
