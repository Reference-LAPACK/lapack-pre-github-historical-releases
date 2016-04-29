.TH  CSWAP 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CSWAP - two vectors
.SH SYNOPSIS
.TP 36
SUBROUTINE CSWAP(N,CX,INCX,CY,INCY)
.TP 36
.ti +4
INTEGER
INCX,INCY,N
.TP 36
.ti +4
COMPLEX
CX(*),CY(*)
.SH PURPOSE
   interchanges two vectors.
   jack dongarra, linpack, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


