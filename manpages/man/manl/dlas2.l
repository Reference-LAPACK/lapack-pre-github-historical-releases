.TH  DLAS2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAS2 - the singular values of the 2-by-2 matrix  [ F G ]  [ 0 H ]
.SH SYNOPSIS
.TP 18
SUBROUTINE DLAS2(
F, G, H, SSMIN, SSMAX )
.TP 18
.ti +4
DOUBLE
PRECISION F, G, H, SSMAX, SSMIN
.SH PURPOSE
DLAS2  computes the singular values of the 2-by-2 matrix
   [  F   G  ]
   [  0   H  ].
On return, SSMIN is the smaller singular value and SSMAX is the
larger singular value.
.br

.SH ARGUMENTS
.TP 8
F       (input) DOUBLE PRECISION
The (1,1) element of the 2-by-2 matrix.
.TP 8
G       (input) DOUBLE PRECISION
The (1,2) element of the 2-by-2 matrix.
.TP 8
H       (input) DOUBLE PRECISION
The (2,2) element of the 2-by-2 matrix.
.TP 8
SSMIN   (output) DOUBLE PRECISION
The smaller singular value.
.TP 8
SSMAX   (output) DOUBLE PRECISION
The larger singular value.
.SH FURTHER DETAILS
Barring over/underflow, all output quantities are correct to within
a few units in the last place (ulps), even in the absence of a guard
digit in addition/subtraction.
.br

In IEEE arithmetic, the code works correctly if one matrix element is
infinite.
.br

Overflow will not occur unless the largest singular value itself
overflows, or is within a few ulps of overflow. (On machines with
partial overflow, like the Cray, overflow may occur if the largest
singular value is within a factor of 2 of overflow.)
.br

Underflow is harmless if underflow is gradual. Otherwise, results
may correspond to a matrix modified by perturbations of size near
the underflow threshold.
.br

