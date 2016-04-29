.TH  CDOTC 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CDOTC - the dot product of two vectors, conjugating the first  vector
.SH SYNOPSIS
.TP 17
COMPLEX FUNCTION
CDOTC(N,CX,INCX,CY,INCY)
.TP 17
.ti +4
INTEGER
INCX,INCY,N
.TP 17
.ti +4
COMPLEX
CX(*),CY(*)
.SH PURPOSE
   forms the dot product of two vectors, conjugating the first
   vector.

.SH FURTHER DETAILS
   jack dongarra, linpack,  3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)

