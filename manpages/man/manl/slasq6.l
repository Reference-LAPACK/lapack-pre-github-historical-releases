.TH  SLASQ6 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLASQ6 - one dqd (shift equal to zero) transform in ping-pong form, with protection against underflow and overflow
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASQ6(
I0, N0, Z, PP, DMIN, DMIN1, DMIN2, DN,
DNM1, DNM2 )
.TP 19
.ti +4
INTEGER
I0, N0, PP
.TP 19
.ti +4
REAL
DMIN, DMIN1, DMIN2, DN, DNM1, DNM2
.TP 19
.ti +4
REAL
Z( * )
.SH PURPOSE
SLASQ6 computes one dqd (shift equal to zero) transform in
ping-pong form, with protection against underflow and overflow.

.SH ARGUMENTS
.TP 6
I0    (input) INTEGER
First index.
.TP 6
N0    (input) INTEGER
Last index.
.TP 6
Z     (input) REAL array, dimension ( 4*N )
Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
an extra argument.
.TP 6
PP    (input) INTEGER
PP=0 for ping, PP=1 for pong.
.TP 6
DMIN  (output) REAL
Minimum value of d.

DMIN1 (output) REAL
Minimum value of d, excluding D( N0 ).

DMIN2 (output) REAL
Minimum value of d, excluding D( N0 ) and D( N0-1 ).
.TP 6
DN    (output) REAL
d(N0), the last value of d.
.TP 6
DNM1  (output) REAL
d(N0-1).
.TP 6
DNM2  (output) REAL
d(N0-2).
