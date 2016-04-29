.TH  DSCAL 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
DSCAL - a vector by a constant
.SH SYNOPSIS
.TP 31
SUBROUTINE DSCAL(N,DA,DX,INCX)
.TP 31
.ti +4
DOUBLE
PRECISION DA
.TP 31
.ti +4
INTEGER
INCX,N
.TP 31
.ti +4
DOUBLE
PRECISION DX(*)
.SH PURPOSE
   scales a vector by a constant.
   uses unrolled loops for increment equal to one.
.br
   jack dongarra, linpack, 3/11/78.
.br
   modified 3/93 to return if incx .le. 0.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


