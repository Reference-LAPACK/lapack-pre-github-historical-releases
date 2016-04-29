.TH  DLAPY2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAPY2 - sqrt(x**2+y**2), taking care not to cause unnecessary overflow
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DLAPY2( X, Y )
.TP 17
.ti +4
DOUBLE
PRECISION X, Y
.SH PURPOSE
DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
overflow.

.SH ARGUMENTS
.TP 8
X       (input) DOUBLE PRECISION
Y       (input) DOUBLE PRECISION
X and Y specify the values x and y.
