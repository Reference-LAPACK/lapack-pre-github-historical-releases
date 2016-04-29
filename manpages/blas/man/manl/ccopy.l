.TH  CCOPY 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CCOPY - copie a vector x to a vector y
.SH SYNOPSIS
.TP 36
SUBROUTINE CCOPY(N,CX,INCX,CY,INCY)
.TP 36
.ti +4
INTEGER
INCX,INCY,N
.TP 36
.ti +4
COMPLEX
CX(*),CY(*)
.SH PURPOSE
   CCOPY copies a vector x to a vector y.

.SH FURTHER DETAILS
   jack dongarra, linpack, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)

