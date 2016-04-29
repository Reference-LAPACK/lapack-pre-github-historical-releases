.TH  CSSCAL 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CSSCAL - a complex vector by a real constant
.SH SYNOPSIS
.TP 32
SUBROUTINE CSSCAL(N,SA,CX,INCX)
.TP 32
.ti +4
REAL
SA
.TP 32
.ti +4
INTEGER
INCX,N
.TP 32
.ti +4
COMPLEX
CX(*)
.SH PURPOSE
   scales a complex vector by a real constant.
   jack dongarra, linpack, 3/11/78.
.br
   modified 3/93 to return if incx .le. 0.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


