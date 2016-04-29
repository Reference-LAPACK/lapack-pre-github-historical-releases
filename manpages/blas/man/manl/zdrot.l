.TH  ZDROT 1 "February 2007" "BLAS routine" "BLAS routine" 
.SH NAME
ZDROT - plane rotation, where the cos and sin (c and s) are real and the vectors cx and cy are complex
.SH SYNOPSIS
.TP 18
SUBROUTINE ZDROT(
N, CX, INCX, CY, INCY, C, S )
.TP 18
.ti +4
INTEGER
INCX, INCY, N
.TP 18
.ti +4
DOUBLE
PRECISION C, S
.TP 18
.ti +4
COMPLEX*16
CX( * ), CY( * )
.SH PURPOSE
Applies a plane rotation, where the cos and sin (c and s) are real
and the vectors cx and cy are complex.
jack dongarra, linpack, 3/11/78.
.br

.SH ARGUMENTS
.TP 9
N        (input) INTEGER
On entry, N specifies the order of the vectors cx and cy.
N must be at least zero.
Unchanged on exit.
.TP 9
CX       (input) COMPLEX*16 array, dimension at least
( 1 + ( N - 1 )*abs( INCX ) ).
Before entry, the incremented array CX must contain the n
element vector cx. On exit, CX is overwritten by the updated
vector cx.
.TP 9
INCX     (input) INTEGER
On entry, INCX specifies the increment for the elements of
CX. INCX must not be zero.
Unchanged on exit.
.TP 9
CY       (input) COMPLEX*16 array, dimension at least
( 1 + ( N - 1 )*abs( INCY ) ).
Before entry, the incremented array CY must contain the n
element vector cy. On exit, CY is overwritten by the updated
vector cy.
.TP 9
INCY     (input) INTEGER
On entry, INCY specifies the increment for the elements of
CY. INCY must not be zero.
Unchanged on exit.
.TP 9
C        (input) DOUBLE PRECISION
On entry, C specifies the cosine, cos.
Unchanged on exit.
.TP 9
S        (input) DOUBLE PRECISION
On entry, S specifies the sine, sin.
Unchanged on exit.
