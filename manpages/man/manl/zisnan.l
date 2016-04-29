.TH  ZISNAN 1 "October 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZISNAN - .TRUE
.SH SYNOPSIS
.TP 17
FUNCTION ZISNAN(
ZIN )
.TP 17
.ti +4
LOGICAL
ZISNAN
.TP 17
.ti +4
COMPLEX*16
ZIN
.SH PURPOSE
DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
otherwise.  To be replaced by the Fortran 2003 intrinsic in the
future.
.br

.SH ARGUMENTS
.TP 9
ZIN      (input) COMPLEX*16
Input to test for NaN.
