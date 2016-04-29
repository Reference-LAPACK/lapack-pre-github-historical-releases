.TH  ZLADIV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLADIV - X / Y, where X and Y are complex
.SH SYNOPSIS
.TP 20
COMPLEX*16 FUNCTION
ZLADIV( X, Y )
.TP 20
.ti +4
COMPLEX*16
X, Y
.SH PURPOSE
ZLADIV := X / Y, where X and Y are complex.  The computation of X / Y
will not overflow on an intermediary step unless the results
overflows.
.br

.SH ARGUMENTS
.TP 8
X       (input) COMPLEX*16
Y       (input) COMPLEX*16
The complex scalars X and Y.
