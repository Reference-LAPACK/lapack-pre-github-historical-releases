.TH  DLARUV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARUV - a vector of n random real numbers from a uniform (0,1)
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARUV(
ISEED, N, X )
.TP 19
.ti +4
INTEGER
N
.TP 19
.ti +4
INTEGER
ISEED( 4 )
.TP 19
.ti +4
DOUBLE
PRECISION X( N )
.SH PURPOSE
DLARUV returns a vector of n random real numbers from a uniform (0,1)
distribution (n <= 128).
.br

This is an auxiliary routine called by DLARNV and ZLARNV.

.SH ARGUMENTS
.TP 8
ISEED   (input/output) INTEGER array, dimension (4)
On entry, the seed of the random number generator; the array
elements must be between 0 and 4095, and ISEED(4) must be
odd.
On exit, the seed is updated.
.TP 8
N       (input) INTEGER
The number of random numbers to be generated. N <= 128.
.TP 8
X       (output) DOUBLE PRECISION array, dimension (N)
The generated random numbers.
.SH FURTHER DETAILS
This routine uses a multiplicative congruential method with modulus
2**48 and multiplier 33952834046453 (see G.S.Fishman,
.br
\(aqMultiplicative congruential random number generators with modulus
2**b: an exhaustive analysis for b = 32 and a partial analysis for
b = 48\(aq, Math. Comp. 189, pp 331-344, 1990).
.br

48-bit integers are stored in 4 integer array elements with 12 bits
per element. Hence the routine is portable across machines with
integers of 32 bits or more.
.br

