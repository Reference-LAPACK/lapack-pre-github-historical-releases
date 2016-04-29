.TH  DISNAN 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DISNAN - .TRUE
.SH SYNOPSIS
.TP 17
LOGICAL FUNCTION
DISNAN(DIN)
.TP 17
.ti +4
DOUBLE
PRECISION DIN
.SH PURPOSE
DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
otherwise.  To be replaced by the Fortran 2003 intrinsic in the
future.
.br

.SH ARGUMENTS
.TP 9
DIN      (input) DOUBLE PRECISION
Input to test for NaN.
