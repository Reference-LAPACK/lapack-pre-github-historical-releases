.TH  ZAXPY 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
ZAXPY - time a vector plus a vector
.SH SYNOPSIS
.TP 39
SUBROUTINE ZAXPY(N,ZA,ZX,INCX,ZY,INCY)
.TP 39
.ti +4
DOUBLE
COMPLEX ZA
.TP 39
.ti +4
INTEGER
INCX,INCY,N
.TP 39
.ti +4
DOUBLE
COMPLEX ZX(*),ZY(*)
.SH PURPOSE
   constant times a vector plus a vector.
   jack dongarra, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


