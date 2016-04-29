.TH  DROTM 1 "February 2007" "BLAS routine" "BLAS routine" 
.SH NAME
DROTM - THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX   (DX**T) , WHERE **T INDICATES TRANSPOSE
.SH SYNOPSIS
.TP 43
SUBROUTINE DROTM(N,DX,INCX,DY,INCY,DPARAM)
.TP 43
.ti +4
INTEGER
INCX,INCY,N
.TP 43
.ti +4
DOUBLE
PRECISION DPARAM(5),DX(1),DY(1)
.SH PURPOSE
   APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
   (DY**T)
.br

   DX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
   LX = (-INCX)*N, AND SIMILARLY FOR SY USING LY AND INCY.
   WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..

   DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0

     (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
   H=(          )    (          )    (          )    (          )
     (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
   SEE DROTMG FOR A DESCRIPTION OF DATA STORAGE IN DPARAM.

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
number of elements in input vector(s)
.TP 7
DX     (input/output) DOUBLE PRECISION array, dimension N
double precision vector with 5 elements
.TP 7
INCX   (input) INTEGER
storage spacing between elements of DX
.TP 7
DY     (input/output) DOUBLE PRECISION array, dimension N
double precision vector with N elements
.TP 7
INCY   (input) INTEGER
storage spacing between elements of DY

DPARAM (input/output)  DOUBLE PRECISION array, dimension 5 
DPARAM(1)=DFLAG
DPARAM(2)=DH11
DPARAM(3)=DH21
DPARAM(4)=DH12
DPARAM(5)=DH22
