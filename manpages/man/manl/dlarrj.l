.TH  DLARRJ 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARRJ - initial eigenvalue approximations of T, DLARRJ does bisection to refine the eigenvalues of T,
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARRJ(
N, D, E2, IFIRST, ILAST,
RTOL, OFFSET, W, WERR, WORK, IWORK,
PIVMIN, SPDIAM, INFO )
.TP 19
.ti +4
INTEGER
IFIRST, ILAST, INFO, N, OFFSET
.TP 19
.ti +4
DOUBLE
PRECISION PIVMIN, RTOL, SPDIAM
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E2( * ), W( * ),
WERR( * ), WORK( * )
.SH PURPOSE
Given the initial eigenvalue approximations of T, DLARRJ
does  bisection to refine the eigenvalues of T,
W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
guesses for these eigenvalues are input in W, the corresponding estimate
of the error in these guesses in WERR. During bisection, intervals
[left, right] are maintained by storing their mid-points and
semi-widths in the arrays W and WERR respectively.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The N diagonal elements of T.
.TP 8
E2      (input) DOUBLE PRECISION array, dimension (N-1)
The Squares of the (N-1) subdiagonal elements of T.
.TP 8
IFIRST  (input) INTEGER
The index of the first eigenvalue to be computed.
.TP 8
ILAST   (input) INTEGER
The index of the last eigenvalue to be computed.
.TP 7
RTOL   (input) DOUBLE PRECISION
Tolerance for the convergence of the bisection intervals.
An interval [LEFT,RIGHT] has converged if
RIGHT-LEFT.LT.RTOL*MAX(|LEFT|,|RIGHT|).
.TP 8
OFFSET  (input) INTEGER
Offset for the arrays W and WERR, i.e., the IFIRST-OFFSET
through ILAST-OFFSET elements of these arrays are to be used.
.TP 8
W       (input/output) DOUBLE PRECISION array, dimension (N)
On input, W( IFIRST-OFFSET ) through W( ILAST-OFFSET ) are
estimates of the eigenvalues of L D L^T indexed IFIRST through
ILAST.
On output, these estimates are refined.
.TP 8
WERR    (input/output) DOUBLE PRECISION array, dimension (N)
On input, WERR( IFIRST-OFFSET ) through WERR( ILAST-OFFSET ) are
the errors in the estimates of the corresponding elements in W.
On output, these errors are refined.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
Workspace.
.TP 8
IWORK   (workspace) INTEGER array, dimension (2*N)
Workspace.
.TP 8
PIVMIN  (input) DOUBLE PRECISION
The minimum pivot in the Sturm sequence for T.
.TP 8
SPDIAM  (input) DOUBLE PRECISION
The spectral diameter of T.
.TP 8
INFO    (output) INTEGER
Error flag.
.SH FURTHER DETAILS
Based on contributions by
.br
   Beresford Parlett, University of California, Berkeley, USA
   Jim Demmel, University of California, Berkeley, USA
.br
   Inderjit Dhillon, University of Texas, Austin, USA
.br
   Osni Marques, LBNL/NERSC, USA
.br
   Christof Voemel, University of California, Berkeley, USA

