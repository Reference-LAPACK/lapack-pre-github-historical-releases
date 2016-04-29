.TH  DLASV2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASV2 - the singular value decomposition of a 2-by-2 triangular matrix  [ F G ]  [ 0 H ]
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASV2(
F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL )
.TP 19
.ti +4
DOUBLE
PRECISION CSL, CSR, F, G, H, SNL, SNR, SSMAX, SSMIN
.SH PURPOSE
DLASV2 computes the singular value decomposition of a 2-by-2
triangular matrix
   [  F   G  ]
   [  0   H  ].
On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
right singular vectors for abs(SSMAX), giving the decomposition

   [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
   [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].

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
abs(SSMIN) is the smaller singular value.
.TP 8
SSMAX   (output) DOUBLE PRECISION
abs(SSMAX) is the larger singular value.
.TP 8
SNL     (output) DOUBLE PRECISION
CSL     (output) DOUBLE PRECISION
The vector (CSL, SNL) is a unit left singular vector for the
singular value abs(SSMAX).
.TP 8
SNR     (output) DOUBLE PRECISION
CSR     (output) DOUBLE PRECISION
The vector (CSR, SNR) is a unit right singular vector for the
singular value abs(SSMAX).
.SH FURTHER DETAILS
Any input parameter may be aliased with any output parameter.

Barring over/underflow and assuming a guard digit in subtraction, all
output quantities are correct to within a few units in the last
place (ulps).
.br

In IEEE arithmetic, the code works correctly if one matrix element is
infinite.
.br

Overflow will not occur unless the largest singular value itself
overflows or is within a few ulps of overflow. (On machines with
partial overflow, like the Cray, overflow may occur if the largest
singular value is within a factor of 2 of overflow.)
.br

Underflow is harmless if underflow is gradual. Otherwise, results
may correspond to a matrix modified by perturbations of size near
the underflow threshold.
.br

