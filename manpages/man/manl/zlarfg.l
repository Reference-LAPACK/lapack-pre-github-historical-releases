.TH  ZLARFG 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLARFG - a complex elementary reflector H of order n, such that   H\(aq * ( alpha ) = ( beta ), H\(aq * H = I
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLARFG(
N, ALPHA, X, INCX, TAU )
.TP 19
.ti +4
INTEGER
INCX, N
.TP 19
.ti +4
COMPLEX*16
ALPHA, TAU
.TP 19
.ti +4
COMPLEX*16
X( * )
.SH PURPOSE
ZLARFG generates a complex elementary reflector H of order n, such
that
           (   x   )   (   0  )
.br

where alpha and beta are scalars, with beta real, and x is an
(n-1)-element complex vector. H is represented in the form

      H = I - tau * ( 1 ) * ( 1 v\(aq ) ,
.br
                    ( v )
.br

where tau is a complex scalar and v is a complex (n-1)-element
vector. Note that H is not hermitian.
.br

If the elements of x are all zero and alpha is real, then tau = 0
and H is taken to be the unit matrix.
.br

Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 .
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the elementary reflector.
.TP 8
ALPHA   (input/output) COMPLEX*16
On entry, the value alpha.
On exit, it is overwritten with the value beta.
.TP 8
X       (input/output) COMPLEX*16 array, dimension
(1+(N-2)*abs(INCX))
On entry, the vector x.
On exit, it is overwritten with the vector v.
.TP 8
INCX    (input) INTEGER
The increment between elements of X. INCX > 0.
.TP 8
TAU     (output) COMPLEX*16
The value tau.
