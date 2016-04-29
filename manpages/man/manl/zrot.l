.TH  ZROT 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZROT - a plane rotation, where the cos (C) is real and the sin (S) is complex, and the vectors CX and CY are complex
.SH SYNOPSIS
.TP 17
SUBROUTINE ZROT(
N, CX, INCX, CY, INCY, C, S )
.TP 17
.ti +4
INTEGER
INCX, INCY, N
.TP 17
.ti +4
DOUBLE
PRECISION C
.TP 17
.ti +4
COMPLEX*16
S
.TP 17
.ti +4
COMPLEX*16
CX( * ), CY( * )
.SH PURPOSE
ZROT   applies a plane rotation, where the cos (C) is real and the
sin (S) is complex, and the vectors CX and CY are complex.

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of elements in the vectors CX and CY.
.TP 8
CX      (input/output) COMPLEX*16 array, dimension (N)
On input, the vector X.
On output, CX is overwritten with C*X + S*Y.
.TP 8
INCX    (input) INTEGER
The increment between successive values of CY.  INCX <> 0.
.TP 8
CY      (input/output) COMPLEX*16 array, dimension (N)
On input, the vector Y.
On output, CY is overwritten with -CONJG(S)*X + C*Y.
.TP 8
INCY    (input) INTEGER
The increment between successive values of CY.  INCX <> 0.
.TP 8
C       (input) DOUBLE PRECISION
S       (input) COMPLEX*16
C and S define a rotation
[  C          S  ]
[ -conjg(S)   C  ]
where C*C + S*CONJG(S) = 1.0.
