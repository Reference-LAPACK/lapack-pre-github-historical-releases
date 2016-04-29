.TH  DLABAD 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLABAD - a input the values computed by DLAMCH for underflow and overflow, and returns the square root of each of these values if the log of LARGE is sufficiently large
.SH SYNOPSIS
.TP 19
SUBROUTINE DLABAD(
SMALL, LARGE )
.TP 19
.ti +4
DOUBLE
PRECISION LARGE, SMALL
.SH PURPOSE
DLABAD takes as input the values computed by DLAMCH for underflow and
overflow, and returns the square root of each of these values if the
log of LARGE is sufficiently large.  This subroutine is intended to
identify machines with a large exponent range, such as the Crays, and
redefine the underflow and overflow limits to be the square roots of
the values computed by DLAMCH.  This subroutine is needed because
DLAMCH does not compensate for poor arithmetic in the upper half of
the exponent range, as is found on a Cray.
.br

.SH ARGUMENTS
.TP 8
SMALL   (input/output) DOUBLE PRECISION
On entry, the underflow threshold as computed by DLAMCH.
On exit, if LOG10(LARGE) is sufficiently large, the square
root of SMALL, otherwise unchanged.
.TP 8
LARGE   (input/output) DOUBLE PRECISION
On entry, the overflow threshold as computed by DLAMCH.
On exit, if LOG10(LARGE) is sufficiently large, the square
root of LARGE, otherwise unchanged.
