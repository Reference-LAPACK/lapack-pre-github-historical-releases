.TH  DLARRK 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARRK - one eigenvalue of a symmetric tridiagonal matrix T to suitable accuracy
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARRK(
N, IW, GL, GU,
D, E2, PIVMIN, RELTOL, W, WERR, INFO)
.TP 19
.ti +4
IMPLICIT
NONE
.TP 19
.ti +4
INTEGER
INFO, IW, N
.TP 19
.ti +4
DOUBLE
PRECISION PIVMIN, RELTOL, GL, GU, W, WERR
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E2( * )
.SH PURPOSE
DLARRK computes one eigenvalue of a symmetric tridiagonal
matrix T to suitable accuracy. This is an auxiliary code to be
called from DSTEMR.
.br

To avoid overflow, the matrix must be scaled so that its
.br
largest element is no greater than overflow**(1/2) *
.br
underflow**(1/4) in absolute value, and for greatest
.br
accuracy, it should not be much smaller than that.
.br

See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
Matrix", Report CS41, Computer Science Dept., Stanford
.br
University, July 21, 1966.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the tridiagonal matrix T.  N >= 0.
.TP 8
IW      (input) INTEGER
The index of the eigenvalues to be returned.
.TP 8
GL      (input) DOUBLE PRECISION
GU      (input) DOUBLE PRECISION
An upper and a lower bound on the eigenvalue.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The n diagonal elements of the tridiagonal matrix T.
.TP 8
E2      (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) squared off-diagonal elements of the tridiagonal matrix T.
.TP 8
PIVMIN  (input) DOUBLE PRECISION
The minimum pivot allowed in the Sturm sequence for T.
.TP 8
RELTOL  (input) DOUBLE PRECISION
The minimum relative width of an interval.  When an interval
is narrower than RELTOL times the larger (in
magnitude) endpoint, then it is considered to be
sufficiently small, i.e., converged.  Note: this should
always be at least radix*machine epsilon.
.TP 8
W       (output) DOUBLE PRECISION
.TP 8
WERR    (output) DOUBLE PRECISION
The error bound on the corresponding eigenvalue approximation
in W.
.TP 8
INFO    (output) INTEGER
= 0:       Eigenvalue converged
.br
= -1:      Eigenvalue did NOT converge
.SH PARAMETERS
.TP 8
FUDGE   DOUBLE PRECISION, default = 2
A "fudge factor" to widen the Gershgorin intervals.
