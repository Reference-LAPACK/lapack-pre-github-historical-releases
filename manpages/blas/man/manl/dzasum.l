.TH  DZASUM 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
DZASUM - the sum of the absolute values
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DZASUM(N,ZX,INCX)
.TP 17
.ti +4
INTEGER
INCX,N
.TP 17
.ti +4
DOUBLE
COMPLEX ZX(*)
.SH PURPOSE
   takes the sum of the absolute values.
   jack dongarra, 3/11/78.
.br
   modified 3/93 to return if incx .le. 0.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


