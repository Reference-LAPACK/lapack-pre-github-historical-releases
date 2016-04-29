.TH  DLASD4 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASD4 - compute the square root of the I-th updated eigenvalue of a positive symmetric rank-one modification to a positive diagonal matrix whose entries are given as the squares of the corresponding entries in the array d, and that   0 <= D(i) < D(j) for i < j  and that RHO > 0
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASD4(
N, I, D, Z, DELTA, RHO, SIGMA, WORK, INFO )
.TP 19
.ti +4
INTEGER
I, INFO, N
.TP 19
.ti +4
DOUBLE
PRECISION RHO, SIGMA
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), DELTA( * ), WORK( * ), Z( * )
.SH PURPOSE
This subroutine computes the square root of the I-th updated
eigenvalue of a positive symmetric rank-one modification to
a positive diagonal matrix whose entries are given as the squares
of the corresponding entries in the array d, and that
no loss in generality.  The rank-one modified system is thus

       diag( D ) * diag( D ) +  RHO *  Z * Z_transpose.
.br

where we assume the Euclidean norm of Z is 1.
.br

The method consists of approximating the rational functions in the
secular equation by simpler interpolating rational functions.

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
The length of all arrays.
.TP 7
I      (input) INTEGER
The index of the eigenvalue to be computed.  1 <= I <= N.
.TP 7
D      (input) DOUBLE PRECISION array, dimension ( N )
The original eigenvalues.  It is assumed that they are in
order, 0 <= D(I) < D(J)  for I < J.
.TP 7
Z      (input) DOUBLE PRECISION array, dimension ( N )
The components of the updating vector.
.TP 7
DELTA  (output) DOUBLE PRECISION array, dimension ( N )
If N .ne. 1, DELTA contains (D(j) - sigma_I) in its  j-th
component.  If N = 1, then DELTA(1) = 1.  The vector DELTA
contains the information necessary to construct the
(singular) eigenvectors.
.TP 7
RHO    (input) DOUBLE PRECISION
The scalar in the symmetric updating formula.
.TP 7
SIGMA  (output) DOUBLE PRECISION
The computed sigma_I, the I-th updated eigenvalue.
.TP 7
WORK   (workspace) DOUBLE PRECISION array, dimension ( N )
If N .ne. 1, WORK contains (D(j) + sigma_I) in its  j-th
component.  If N = 1, then WORK( 1 ) = 1.
.TP 7
INFO   (output) INTEGER
= 0:  successful exit
.br
> 0:  if INFO = 1, the updating process failed.
.SH PARAMETERS

Logical variable ORGATI (origin-at-i?) is used for distinguishing
whether D(i) or D(i+1) is treated as the origin.

ORGATI = .true.    origin at i
ORGATI = .false.   origin at i+1

Logical variable SWTCH3 (switch-for-3-poles?) is for noting
if we are working with THREE poles!

MAXIT is the maximum number of iterations allowed for each
eigenvalue.

Further Details
===============

Based on contributions by
Ren-Cang Li, Computer Science Division, University of California
at Berkeley, USA
