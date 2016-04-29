.TH  DLAED6 1 "February 2007" " LAPACK routine (version 3.1.1) " " LAPACK routine (version 3.1.1) " 
.SH NAME
DLAED6 - the positive or negative root (closest to the origin) of  z(1) z(2) z(3) f(x) = rho + --------- + ---------- + ---------  d(1)-x d(2)-x d(3)-x  It is assumed that   if ORGATI = .true
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAED6(
KNITER, ORGATI, RHO, D, Z, FINIT, TAU, INFO )
.TP 19
.ti +4
LOGICAL
ORGATI
.TP 19
.ti +4
INTEGER
INFO, KNITER
.TP 19
.ti +4
DOUBLE
PRECISION FINIT, RHO, TAU
.TP 19
.ti +4
DOUBLE
PRECISION D( 3 ), Z( 3 )
.SH PURPOSE
DLAED6 computes the positive or negative root (closest to the origin)
of
                 z(1)        z(2)        z(3)
f(x) =   rho + --------- + ---------- + ---------
                d(1)-x      d(2)-x      d(3)-x
      otherwise it is between d(1) and d(2)
.br

This routine will be called by DLAED4 when necessary. In most cases,
the root sought is the smallest in magnitude, though it might not be
in some extremely rare situations.
.br

.SH ARGUMENTS
.TP 13
KNITER       (input) INTEGER
Refer to DLAED4 for its significance.
.TP 13
ORGATI       (input) LOGICAL
If ORGATI is true, the needed root is between d(2) and
d(3); otherwise it is between d(1) and d(2).  See
DLAED4 for further details.
.TP 13
RHO          (input) DOUBLE PRECISION
Refer to the equation f(x) above.
.TP 13
D            (input) DOUBLE PRECISION array, dimension (3)
D satisfies d(1) < d(2) < d(3).
.TP 13
Z            (input) DOUBLE PRECISION array, dimension (3)
Each of the elements in z must be positive.
.TP 13
FINIT        (input) DOUBLE PRECISION
The value of f at 0. It is more accurate than the one
evaluated inside this routine (if someone wants to do
so).
.TP 13
TAU          (output) DOUBLE PRECISION
The root of the equation f(x).
.TP 13
INFO         (output) INTEGER
= 0: successful exit
.br
> 0: if INFO = 1, failure to converge
.SH FURTHER DETAILS
30/06/99: Based on contributions by
.br
   Ren-Cang Li, Computer Science Division, University of California
   at Berkeley, USA
.br

10/02/03: This version has a few statements commented out for thread
safety (machine parameters are computed on each entry). SJH.

05/10/06: Modified from a new version of Ren-Cang Li, use
   Gragg-Thornton-Warner cubic convergent scheme for better stability.

