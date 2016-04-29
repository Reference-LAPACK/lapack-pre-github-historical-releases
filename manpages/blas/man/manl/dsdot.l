.TH  DSDOT 1 "February 2007" "BLAS routine" "BLAS routine" 
.SH NAME
DSDOT - and result
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DSDOT(N,SX,INCX,SY,INCY)
.TP 17
.ti +4
INTEGER
INCX,INCY,N
.TP 17
.ti +4
REAL
SX(*),SY(*)
.SH PURPOSE
precision accumulation and result.

Returns D.P. dot product accumulated in D.P., for S.P. SX and SY
DSDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * SY(LY+I*INCY),
where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
defined in a similar way using INCY.
.br

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
number of elements in input vector(s)
.TP 7
SX     (input) REAL array, dimension(N)
single precision vector with N elements
.TP 7
INCX   (input) INTEGER
storage spacing between elements of SX
.TP 7
SY     (input) REAL array, dimension(N)
single precision vector with N elements
.TP 7
INCY   (input) INTEGER
storage spacing between elements of SY
.TP 7
DSDOT  (output) DOUBLE PRECISION
DSDOT  double precision dot product (zero if N.LE.0)

REFERENCES
==========
C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
Krogh, Basic linear algebra subprograms for Fortran
usage, Algorithm No. 539, Transactions on Mathematical
Software 5, 3 (September 1979), pp. 308-323.

REVISION HISTORY  (YYMMDD)
==========================
.TP 8
791001  DATE WRITTEN
890831  Modified array declarations.  (WRB)
890831  REVISION DATE from Version 3.2
891214  Prologue converted to Version 4.0 format.  (BAB)
920310  Corrected definition of LX in DESCRIPTION.  (WRB)
920501  Reformatted the REFERENCES section.  (WRB)
070118  Reformat to LAPACK style (JL)
