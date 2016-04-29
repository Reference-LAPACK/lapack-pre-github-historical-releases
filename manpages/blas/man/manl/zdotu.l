.TH  ZDOTU 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
ZDOTU - form the dot product of two vectors
.SH SYNOPSIS
.TP 15
DOUBLE COMPLEX
FUNCTION ZDOTU(N,ZX,INCX,ZY,INCY)
.TP 15
.ti +4
INTEGER
INCX,INCY,N
.TP 15
.ti +4
DOUBLE
COMPLEX ZX(*),ZY(*)
.SH PURPOSE
   ZDOTU forms the dot product of two vectors.

.SH FURTHER DETAILS
   jack dongarra, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)

