.TH  DLAPY3 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAPY3 - sqrt(x**2+y**2+z**2), taking care not to cause unnecessary overflow
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DLAPY3( X, Y, Z )
.TP 17
.ti +4
DOUBLE
PRECISION X, Y, Z
.SH PURPOSE
DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
unnecessary overflow.

.SH ARGUMENTS
.TP 8
X       (input) DOUBLE PRECISION
Y       (input) DOUBLE PRECISION
Z       (input) DOUBLE PRECISION
X, Y and Z specify the values x, y and z.
