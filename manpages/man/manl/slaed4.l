.TH  SLAED4 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLAED4 - compute the I-th updated eigenvalue of a symmetric rank-one modification to a diagonal matrix whose elements are given in the array d, and that   D(i) < D(j) for i < j  and that RHO > 0
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAED4(
N, I, D, Z, DELTA, RHO, DLAM, INFO )
.TP 19
.ti +4
INTEGER
I, INFO, N
.TP 19
.ti +4
REAL
DLAM, RHO
.TP 19
.ti +4
REAL
D( * ), DELTA( * ), Z( * )
.SH PURPOSE
This subroutine computes the I-th updated eigenvalue of a symmetric
rank-one modification to a diagonal matrix whose elements are
given in the array d, and that
no loss in generality.  The rank-one modified system is thus

           diag( D )  +  RHO *  Z * Z_transpose.
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
D      (input) REAL array, dimension (N)
The original eigenvalues.  It is assumed that they are in
order, D(I) < D(J)  for I < J.
.TP 7
Z      (input) REAL array, dimension (N)
The components of the updating vector.
.TP 7
DELTA  (output) REAL array, dimension (N)
If N .GT. 2, DELTA contains (D(j) - lambda_I) in its  j-th
component.  If N = 1, then DELTA(1) = 1. If N = 2, see SLAED5
for detail. The vector DELTA contains the information necessary
to construct the eigenvectors by SLAED3 and SLAED9.
.TP 7
RHO    (input) REAL
The scalar in the symmetric updating formula.
.TP 7
DLAM   (output) REAL
The computed lambda_I, the I-th updated eigenvalue.
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
