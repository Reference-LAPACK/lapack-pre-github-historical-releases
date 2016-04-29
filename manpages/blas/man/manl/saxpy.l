.TH  SAXPY 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
SAXPY - constant times a vector plus a vector
.SH SYNOPSIS
.TP 39
SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)
.TP 39
.ti +4
REAL
SA
.TP 39
.ti +4
INTEGER
INCX,INCY,N
.TP 39
.ti +4
REAL
SX(*),SY(*)
.SH PURPOSE
   SAXPY constant times a vector plus a vector.
   uses unrolled loop for increments equal to one.
.br
   jack dongarra, linpack, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)


