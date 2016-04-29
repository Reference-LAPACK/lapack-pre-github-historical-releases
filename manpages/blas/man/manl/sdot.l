.TH  SDOT 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
SDOT - the dot product of two vectors
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SDOT(N,SX,INCX,SY,INCY)
.TP 14
.ti +4
INTEGER
INCX,INCY,N
.TP 14
.ti +4
REAL
SX(*),SY(*)
.SH PURPOSE
   forms the dot product of two vectors.
   uses unrolled loops for increments equal to one.
.br
   jack dongarra, linpack, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)



