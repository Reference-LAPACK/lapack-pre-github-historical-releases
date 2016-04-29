.TH  CSCAL 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CSCAL - a vector by a constant
.SH SYNOPSIS
.TP 31
SUBROUTINE CSCAL(N,CA,CX,INCX)
.TP 31
.ti +4
COMPLEX
CA
.TP 31
.ti +4
INTEGER
INCX,N
.TP 31
.ti +4
COMPLEX
CX(*)
.SH PURPOSE
   scales a vector by a constant.
   jack dongarra, linpack,  3/11/78.
.br
   modified 3/93 to return if incx .le. 0.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


