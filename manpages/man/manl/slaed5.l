.TH  SLAED5 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLAED5 - compute the I-th eigenvalue of a symmetric rank-one modification of a 2-by-2 diagonal matrix   diag( D ) + RHO  The diagonal elements in the array D are assumed to satisfy   D(i) < D(j) for i < j 
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAED5(
I, D, Z, DELTA, RHO, DLAM )
.TP 19
.ti +4
INTEGER
I
.TP 19
.ti +4
REAL
DLAM, RHO
.TP 19
.ti +4
REAL
D( 2 ), DELTA( 2 ), Z( 2 )
.SH PURPOSE
This subroutine computes the I-th eigenvalue of a symmetric rank-one
modification of a 2-by-2 diagonal matrix

We also assume RHO > 0 and that the Euclidean norm of the vector
Z is one.
.br

.SH ARGUMENTS
.TP 7
I      (input) INTEGER
The index of the eigenvalue to be computed.  I = 1 or I = 2.
.TP 7
D      (input) REAL array, dimension (2)
The original eigenvalues.  We assume D(1) < D(2).
.TP 7
Z      (input) REAL array, dimension (2)
The components of the updating vector.
.TP 7
DELTA  (output) REAL array, dimension (2)
The vector DELTA contains the information necessary
to construct the eigenvectors.
.TP 7
RHO    (input) REAL
The scalar in the symmetric updating formula.
.TP 7
DLAM   (output) REAL
The computed lambda_I, the I-th updated eigenvalue.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ren-Cang Li, Computer Science Division, University of California
   at Berkeley, USA
.br

