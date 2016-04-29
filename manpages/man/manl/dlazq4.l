.TH  DLAZQ4 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAZQ4 - an approximation TAU to the smallest eigenvalue using values of d from the previous transform
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAZQ4(
I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN,
DN1, DN2, TAU, TTYPE, G )
.TP 19
.ti +4
INTEGER
I0, N0, N0IN, PP, TTYPE
.TP 19
.ti +4
DOUBLE
PRECISION DMIN, DMIN1, DMIN2, DN, DN1, DN2, G, TAU
.TP 19
.ti +4
DOUBLE
PRECISION Z( * )
.SH PURPOSE
DLAZQ4 computes an approximation TAU to the smallest eigenvalue 
using values of d from the previous transform.

I0    (input) INTEGER
.br
      First index.
.br

N0    (input) INTEGER
.br
      Last index.
.br

Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
.br
      Z holds the qd array.
.br

PP    (input) INTEGER
.br
      PP=0 for ping, PP=1 for pong.
.br

N0IN  (input) INTEGER
.br
      The value of N0 at start of EIGTEST.
.br

DMIN  (input) DOUBLE PRECISION
.br
      Minimum value of d.
.br

DMIN1 (input) DOUBLE PRECISION
.br
      Minimum value of d, excluding D( N0 ).
.br

DMIN2 (input) DOUBLE PRECISION
.br
      Minimum value of d, excluding D( N0 ) and D( N0-1 ).

DN    (input) DOUBLE PRECISION
.br
      d(N)
.br

DN1   (input) DOUBLE PRECISION
.br
      d(N-1)
.br

DN2   (input) DOUBLE PRECISION
.br
      d(N-2)
.br

TAU   (output) DOUBLE PRECISION
.br
      This is the shift.
.br

TTYPE (output) INTEGER
.br
      Shift type.
.br

G     (input/output) DOUBLE PRECISION
.br
      G is passed as an argument in order to save its value between
      calls to DLAZQ4
.br

.SH FURTHER DETAILS

This is a thread safe version of DLASQ4, which passes G through the
argument list in place of declaring G in a SAVE statment.

