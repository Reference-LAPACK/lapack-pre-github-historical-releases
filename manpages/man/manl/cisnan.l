.TH  CISNAN 1 "October 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CISNAN - .TRUE
.SH SYNOPSIS
.TP 17
FUNCTION CISNAN(
CIN )
.TP 17
.ti +4
LOGICAL
CISNAN
.TP 17
.ti +4
COMPLEX
CIN
.SH PURPOSE
SISNAN returns .TRUE. if its argument is NaN, and .FALSE.
otherwise.  To be replaced by the Fortran 2003 intrinsic in the
future.
.br

.SH ARGUMENTS
.TP 9
CIN      (input) COMPLEX
Input to test for NaN.
