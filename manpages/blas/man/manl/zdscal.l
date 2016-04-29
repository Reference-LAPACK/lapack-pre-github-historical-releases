.TH  ZDSCAL 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
ZDSCAL - a vector by a constant
.SH SYNOPSIS
.TP 32
SUBROUTINE ZDSCAL(N,DA,ZX,INCX)
.TP 32
.ti +4
DOUBLE
PRECISION DA
.TP 32
.ti +4
INTEGER
INCX,N
.TP 32
.ti +4
DOUBLE
COMPLEX ZX(*)
.SH PURPOSE
   scales a vector by a constant.
   jack dongarra, 3/11/78.
.br
   modified 3/93 to return if incx .le. 0.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


