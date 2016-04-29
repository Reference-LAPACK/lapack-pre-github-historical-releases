.TH  CLARNV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLARNV - a vector of n random complex numbers from a uniform or normal distribution
.SH SYNOPSIS
.TP 19
SUBROUTINE CLARNV(
IDIST, ISEED, N, X )
.TP 19
.ti +4
INTEGER
IDIST, N
.TP 19
.ti +4
INTEGER
ISEED( 4 )
.TP 19
.ti +4
COMPLEX
X( * )
.SH PURPOSE
CLARNV returns a vector of n random complex numbers from a uniform or
normal distribution.

.SH ARGUMENTS
.TP 8
IDIST   (input) INTEGER
Specifies the distribution of the random numbers:
.br
= 1:  real and imaginary parts each uniform (0,1)
.br
= 2:  real and imaginary parts each uniform (-1,1)
.br
= 3:  real and imaginary parts each normal (0,1)
.br
= 4:  uniformly distributed on the disc abs(z) < 1
.br
= 5:  uniformly distributed on the circle abs(z) = 1
.TP 8
ISEED   (input/output) INTEGER array, dimension (4)
On entry, the seed of the random number generator; the array
elements must be between 0 and 4095, and ISEED(4) must be
odd.
On exit, the seed is updated.
.TP 8
N       (input) INTEGER
The number of random numbers to be generated.
.TP 8
X       (output) COMPLEX array, dimension (N)
The generated random numbers.
.SH FURTHER DETAILS
This routine calls the auxiliary routine SLARUV to generate random
real numbers from a uniform (0,1) distribution, in batches of up to
128 using vectorisable code. The Box-Muller method is used to
transform numbers from a uniform to a normal distribution.

