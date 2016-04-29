.TH  ZLACRT 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLACRT - the operation   ( c s )( x ) ==> ( x )  ( -s c )( y ) ( y )  where c and s are complex and the vectors x and y are complex
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLACRT(
N, CX, INCX, CY, INCY, C, S )
.TP 19
.ti +4
INTEGER
INCX, INCY, N
.TP 19
.ti +4
COMPLEX*16
C, S
.TP 19
.ti +4
COMPLEX*16
CX( * ), CY( * )
.SH PURPOSE
ZLACRT performs the operation

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of elements in the vectors CX and CY.
.TP 8
CX      (input/output) COMPLEX*16 array, dimension (N)
On input, the vector x.
On output, CX is overwritten with c*x + s*y.
.TP 8
INCX    (input) INTEGER
The increment between successive values of CX.  INCX <> 0.
.TP 8
CY      (input/output) COMPLEX*16 array, dimension (N)
On input, the vector y.
On output, CY is overwritten with -s*x + c*y.
.TP 8
INCY    (input) INTEGER
The increment between successive values of CY.  INCY <> 0.
.TP 8
C       (input) COMPLEX*16
S       (input) COMPLEX*16
C and S define the matrix
[  C   S  ].
[ -S   C  ]
