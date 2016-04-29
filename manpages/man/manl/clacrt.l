.TH  CLACRT 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLACRT - the operation   ( c s )( x ) ==> ( x )  ( -s c )( y ) ( y )  where c and s are complex and the vectors x and y are complex
.SH SYNOPSIS
.TP 19
SUBROUTINE CLACRT(
N, CX, INCX, CY, INCY, C, S )
.TP 19
.ti +4
INTEGER
INCX, INCY, N
.TP 19
.ti +4
COMPLEX
C, S
.TP 19
.ti +4
COMPLEX
CX( * ), CY( * )
.SH PURPOSE
CLACRT performs the operation

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of elements in the vectors CX and CY.
.TP 8
CX      (input/output) COMPLEX array, dimension (N)
On input, the vector x.
On output, CX is overwritten with c*x + s*y.
.TP 8
INCX    (input) INTEGER
The increment between successive values of CX.  INCX <> 0.
.TP 8
CY      (input/output) COMPLEX array, dimension (N)
On input, the vector y.
On output, CY is overwritten with -s*x + c*y.
.TP 8
INCY    (input) INTEGER
The increment between successive values of CY.  INCY <> 0.
.TP 8
C       (input) COMPLEX
S       (input) COMPLEX
C and S define the matrix
[  C   S  ].
[ -S   C  ]
