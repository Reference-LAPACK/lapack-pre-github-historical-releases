.TH  CAXPY 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CAXPY - constant times a vector plus a vector
.SH SYNOPSIS
.TP 39
SUBROUTINE CAXPY(N,CA,CX,INCX,CY,INCY)
.TP 39
.ti +4
COMPLEX
CA
.TP 39
.ti +4
INTEGER
INCX,INCY,N
.TP 39
.ti +4
COMPLEX
CX(*),CY(*)
.SH PURPOSE
   CAXPY constant times a vector plus a vector.

.SH FURTHER DETAILS
   jack dongarra, linpack, 3/11/78.
.br
   modified 12/3/93, array(1) declarations changed to array(*)

