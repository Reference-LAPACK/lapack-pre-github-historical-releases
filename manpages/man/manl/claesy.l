.TH  CLAESY 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLAESY - the eigendecomposition of a 2-by-2 symmetric matrix  ( ( A, B );( B, C ) ) provided the norm of the matrix of eigenvectors is larger than some threshold value
.SH SYNOPSIS
.TP 19
SUBROUTINE CLAESY(
A, B, C, RT1, RT2, EVSCAL, CS1, SN1 )
.TP 19
.ti +4
COMPLEX
A, B, C, CS1, EVSCAL, RT1, RT2, SN1
.SH PURPOSE
CLAESY computes the eigendecomposition of a 2-by-2 symmetric matrix
   ( ( A, B );( B, C ) )
provided the norm of the matrix of eigenvectors is larger than
some threshold value.

RT1 is the eigenvalue of larger absolute value, and RT2 of
smaller absolute value.  If the eigenvectors are computed, then
on return ( CS1, SN1 ) is the unit eigenvector for RT1, hence

[  CS1     SN1   ] . [ A  B ] . [ CS1    -SN1   ] = [ RT1  0  ]
[ -SN1     CS1   ]   [ B  C ]   [ SN1     CS1   ]   [  0  RT2 ]

.SH ARGUMENTS
.TP 8
A       (input) COMPLEX
The ( 1, 1 ) element of input matrix.
.TP 8
B       (input) COMPLEX
The ( 1, 2 ) element of input matrix.  The ( 2, 1 ) element
is also given by B, since the 2-by-2 matrix is symmetric.
.TP 8
C       (input) COMPLEX
The ( 2, 2 ) element of input matrix.
.TP 8
RT1     (output) COMPLEX
The eigenvalue of larger modulus.
.TP 8
RT2     (output) COMPLEX
The eigenvalue of smaller modulus.
.TP 8
EVSCAL  (output) COMPLEX
The complex value by which the eigenvector matrix was scaled
to make it orthonormal.  If EVSCAL is zero, the eigenvectors
were not computed.  This means one of two things:  the 2-by-2
matrix could not be diagonalized, or the norm of the matrix
of eigenvectors before scaling was larger than the threshold
value THRESH (set below).
.TP 8
CS1     (output) COMPLEX
SN1     (output) COMPLEX
If EVSCAL .NE. 0,  ( CS1, SN1 ) is the unit right eigenvector
for RT1.
