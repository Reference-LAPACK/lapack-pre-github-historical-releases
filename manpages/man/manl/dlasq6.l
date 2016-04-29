.TH  DLASQ6 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASQ6 - one dqd (shift equal to zero) transform in ping-pong form, with protection against underflow and overflow
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASQ6(
I0, N0, Z, PP, DMIN, DMIN1, DMIN2, DN,
DNM1, DNM2 )
.TP 19
.ti +4
INTEGER
I0, N0, PP
.TP 19
.ti +4
DOUBLE
PRECISION DMIN, DMIN1, DMIN2, DN, DNM1, DNM2
.TP 19
.ti +4
DOUBLE
PRECISION Z( * )
.SH PURPOSE
DLASQ6 computes one dqd (shift equal to zero) transform in
ping-pong form, with protection against underflow and overflow.

.SH ARGUMENTS
.TP 6
I0    (input) INTEGER
First index.
.TP 6
N0    (input) INTEGER
Last index.
.TP 6
Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
an extra argument.
.TP 6
PP    (input) INTEGER
PP=0 for ping, PP=1 for pong.
.TP 6
DMIN  (output) DOUBLE PRECISION
Minimum value of d.

DMIN1 (output) DOUBLE PRECISION
Minimum value of d, excluding D( N0 ).

DMIN2 (output) DOUBLE PRECISION
Minimum value of d, excluding D( N0 ) and D( N0-1 ).
.TP 6
DN    (output) DOUBLE PRECISION
d(N0), the last value of d.
.TP 6
DNM1  (output) DOUBLE PRECISION
d(N0-1).
.TP 6
DNM2  (output) DOUBLE PRECISION
d(N0-2).
