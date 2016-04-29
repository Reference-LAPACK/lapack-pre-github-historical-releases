.TH  SROTM 1 "February 2007" "BLAS routine" "BLAS routine" 
.SH NAME
SROTM - THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX   (SX**T) , WHERE **T INDICATES TRANSPOSE
.SH SYNOPSIS
.TP 43
SUBROUTINE SROTM(N,SX,INCX,SY,INCY,SPARAM)
.TP 43
.ti +4
INTEGER
INCX,INCY,N
.TP 43
.ti +4
REAL
SPARAM(5),SX(1),SY(1)
.SH PURPOSE
   APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
   (DX**T)
.br

   SX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
   LX = (-INCX)*N, AND SIMILARLY FOR SY USING USING LY AND INCY.
   WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..

   SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0

     (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)
   H=(          )    (          )    (          )    (          )
     (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
   SEE  SROTMG FOR A DESCRIPTION OF DATA STORAGE IN SPARAM.


.SH ARGUMENTS
.TP 7
N      (input) INTEGER
number of elements in input vector(s)
.TP 7
SX     (input/output) REAL array, dimension N
double precision vector with 5 elements
.TP 7
INCX   (input) INTEGER
storage spacing between elements of SX
.TP 7
SY     (input/output) REAL array, dimension N
double precision vector with N elements
.TP 7
INCY   (input) INTEGER
storage spacing between elements of SY

SPARAM (input/output)  REAL array, dimension 5
SPARAM(1)=SFLAG
SPARAM(2)=SH11
SPARAM(3)=SH21
SPARAM(4)=SH12
SPARAM(5)=SH22
