.TH  ZSWAP 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
ZSWAP - two vectors
.SH SYNOPSIS
.TP 36
SUBROUTINE ZSWAP(N,ZX,INCX,ZY,INCY)
.TP 36
.ti +4
INTEGER
INCX,INCY,N
.TP 36
.ti +4
DOUBLE
COMPLEX ZX(*),ZY(*)
.SH PURPOSE
   interchanges two vectors.
   jack dongarra, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


