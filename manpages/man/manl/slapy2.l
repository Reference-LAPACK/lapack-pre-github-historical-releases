.TH  SLAPY2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAPY2 - sqrt(x**2+y**2), taking care not to cause unnecessary overflow
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SLAPY2( X, Y )
.TP 14
.ti +4
REAL
X, Y
.SH PURPOSE
SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
overflow.

.SH ARGUMENTS
.TP 8
X       (input) REAL
Y       (input) REAL
X and Y specify the values x and y.
