.TH  DAXPY 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
DAXPY - time a vector plus a vector
.SH SYNOPSIS
.TP 39
SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
.TP 39
.ti +4
DOUBLE
PRECISION DA
.TP 39
.ti +4
INTEGER
INCX,INCY,N
.TP 39
.ti +4
DOUBLE
PRECISION DX(*),DY(*)
.SH PURPOSE
   constant times a vector plus a vector.
   uses unrolled loops for increments equal to one.
.br
   jack dongarra, linpack, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


