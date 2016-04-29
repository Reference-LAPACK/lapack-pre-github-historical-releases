.TH  DLARFG 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARFG - a real elementary reflector H of order n, such that   H * ( alpha ) = ( beta ), H\(aq * H = I
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARFG(
N, ALPHA, X, INCX, TAU )
.TP 19
.ti +4
INTEGER
INCX, N
.TP 19
.ti +4
DOUBLE
PRECISION ALPHA, TAU
.TP 19
.ti +4
DOUBLE
PRECISION X( * )
.SH PURPOSE
DLARFG generates a real elementary reflector H of order n, such
that
          (   x   )   (   0  )
.br

where alpha and beta are scalars, and x is an (n-1)-element real
vector. H is represented in the form
.br

      H = I - tau * ( 1 ) * ( 1 v\(aq ) ,
.br
                    ( v )
.br

where tau is a real scalar and v is a real (n-1)-element
.br
vector.
.br

If the elements of x are all zero, then tau = 0 and H is taken to be
the unit matrix.
.br

Otherwise  1 <= tau <= 2.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the elementary reflector.
.TP 8
ALPHA   (input/output) DOUBLE PRECISION
On entry, the value alpha.
On exit, it is overwritten with the value beta.
.TP 8
X       (input/output) DOUBLE PRECISION array, dimension
(1+(N-2)*abs(INCX))
On entry, the vector x.
On exit, it is overwritten with the vector v.
.TP 8
INCX    (input) INTEGER
The increment between elements of X. INCX > 0.
.TP 8
TAU     (output) DOUBLE PRECISION
The value tau.
