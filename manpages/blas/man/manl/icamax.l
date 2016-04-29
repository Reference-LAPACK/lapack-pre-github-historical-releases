.TH  ICAMAX 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
ICAMAX - the index of element having max
.SH SYNOPSIS
.TP 17
INTEGER FUNCTION
ICAMAX(N,CX,INCX)
.TP 17
.ti +4
INTEGER
INCX,N
.TP 17
.ti +4
COMPLEX
CX(*)
.SH PURPOSE
   finds the index of element having max. absolute value.
   jack dongarra, linpack, 3/11/78.
.br
   modified 3/93 to return if incx .le. 0.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


