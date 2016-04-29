.TH  DLAZQ3 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAZQ3 - for deflation, computes a shift (TAU) and calls dqds
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAZQ3(
I0, N0, Z, PP, DMIN, SIGMA, DESIG, QMAX, NFAIL,
ITER, NDIV, IEEE, TTYPE, DMIN1, DMIN2, DN, DN1,
DN2, TAU )
.TP 19
.ti +4
LOGICAL
IEEE
.TP 19
.ti +4
INTEGER
I0, ITER, N0, NDIV, NFAIL, PP, TTYPE
.TP 19
.ti +4
DOUBLE
PRECISION DESIG, DMIN, DMIN1, DMIN2, DN, DN1, DN2, QMAX,
SIGMA, TAU
.TP 19
.ti +4
DOUBLE
PRECISION Z( * )
.SH PURPOSE
DLAZQ3 checks for deflation, computes a shift (TAU) and calls dqds.
In case of failure it changes shifts, and tries again until output
is positive.
.br

.SH ARGUMENTS
.TP 7
I0     (input) INTEGER
First index.
.TP 7
N0     (input) INTEGER
Last index.
.TP 7
Z      (input) DOUBLE PRECISION array, dimension ( 4*N )
Z holds the qd array.
.TP 7
PP     (input) INTEGER
PP=0 for ping, PP=1 for pong.
.TP 7
DMIN   (output) DOUBLE PRECISION
Minimum value of d.
.TP 7
SIGMA  (output) DOUBLE PRECISION
Sum of shifts used in current segment.
.TP 7
DESIG  (input/output) DOUBLE PRECISION
Lower order part of SIGMA
.TP 7
QMAX   (input) DOUBLE PRECISION
Maximum value of q.
.TP 7
NFAIL  (output) INTEGER
Number of times shift was too big.
.TP 7
ITER   (output) INTEGER
Number of iterations.
.TP 7
NDIV   (output) INTEGER
Number of divisions.
.TP 7
IEEE   (input) LOGICAL
Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
.TP 7
TTYPE  (input/output) INTEGER
Shift type.  TTYPE is passed as an argument in order to save
its value between calls to DLAZQ3
.TP 7
DMIN1  (input/output) REAL
DMIN2  (input/output) REAL
DN     (input/output) REAL
DN1    (input/output) REAL
DN2    (input/output) REAL
TAU    (input/output) REAL
These are passed as arguments in order to save their values
between calls to DLAZQ3

This is a thread safe version of DLASQ3, which passes TTYPE, DMIN1,
DMIN2, DN, DN1. DN2 and TAU through the argument list in place of
declaring them in a SAVE statment.
