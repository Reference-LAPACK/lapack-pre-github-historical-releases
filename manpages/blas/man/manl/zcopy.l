.TH  ZCOPY 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
ZCOPY - a vector, x, to a vector, y
.SH SYNOPSIS
.TP 36
SUBROUTINE ZCOPY(N,ZX,INCX,ZY,INCY)
.TP 36
.ti +4
INTEGER
INCX,INCY,N
.TP 36
.ti +4
DOUBLE
COMPLEX ZX(*),ZY(*)
.SH PURPOSE
   copies a vector, x, to a vector, y.
   jack dongarra, linpack, 4/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


