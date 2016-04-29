.TH  DLASD5 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASD5 - compute the square root of the I-th eigenvalue of a positive symmetric rank-one modification of a 2-by-2 diagonal matrix   diag( D ) * diag( D ) + RHO  The diagonal entries in the array D are assumed to satisfy   0 <= D(i) < D(j) for i < j 
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASD5(
I, D, Z, DELTA, RHO, DSIGMA, WORK )
.TP 19
.ti +4
INTEGER
I
.TP 19
.ti +4
DOUBLE
PRECISION DSIGMA, RHO
.TP 19
.ti +4
DOUBLE
PRECISION D( 2 ), DELTA( 2 ), WORK( 2 ), Z( 2 )
.SH PURPOSE
This subroutine computes the square root of the I-th eigenvalue
of a positive symmetric rank-one modification of a 2-by-2 diagonal
matrix

We also assume RHO > 0 and that the Euclidean norm of the vector
Z is one.
.br

.SH ARGUMENTS
.TP 7
I      (input) INTEGER
The index of the eigenvalue to be computed.  I = 1 or I = 2.
.TP 7
D      (input) DOUBLE PRECISION array, dimension ( 2 )
The original eigenvalues.  We assume 0 <= D(1) < D(2).
.TP 7
Z      (input) DOUBLE PRECISION array, dimension ( 2 )
The components of the updating vector.
.TP 7
DELTA  (output) DOUBLE PRECISION array, dimension ( 2 )
Contains (D(j) - sigma_I) in its  j-th component.
The vector DELTA contains the information necessary
to construct the eigenvectors.
.TP 7
RHO    (input) DOUBLE PRECISION
The scalar in the symmetric updating formula.

DSIGMA (output) DOUBLE PRECISION
The computed sigma_I, the I-th updated eigenvalue.
.TP 7
WORK   (workspace) DOUBLE PRECISION array, dimension ( 2 )
WORK contains (D(j) + sigma_I) in its  j-th component.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ren-Cang Li, Computer Science Division, University of California
   at Berkeley, USA
.br

