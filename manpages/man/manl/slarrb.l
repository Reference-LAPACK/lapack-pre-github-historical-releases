.TH  SLARRB 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLARRB - relatively robust representation(RRR) L D L^T, SLARRB does "limited" bisection to refine the eigenvalues of L D L^T,
.SH SYNOPSIS
.TP 19
SUBROUTINE SLARRB(
N, D, LLD, IFIRST, ILAST, RTOL1,
RTOL2, OFFSET, W, WGAP, WERR, WORK, IWORK,
PIVMIN, SPDIAM, TWIST, INFO )
.TP 19
.ti +4
INTEGER
IFIRST, ILAST, INFO, N, OFFSET, TWIST
.TP 19
.ti +4
REAL
PIVMIN, RTOL1, RTOL2, SPDIAM
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
D( * ), LLD( * ), W( * ),
WERR( * ), WGAP( * ), WORK( * )
.SH PURPOSE
Given the relatively robust representation(RRR) L D L^T, SLARRB
does "limited" bisection to refine the eigenvalues of L D L^T,
W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
guesses for these eigenvalues are input in W, the corresponding estimate
of the error in these guesses and their gaps are input in WERR
and WGAP, respectively. During bisection, intervals
.br
[left, right] are maintained by storing their mid-points and
semi-widths in the arrays W and WERR respectively.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix.
.TP 8
D       (input) REAL             array, dimension (N)
The N diagonal elements of the diagonal matrix D.
.TP 8
LLD     (input) REAL             array, dimension (N-1)
The (N-1) elements L(i)*L(i)*D(i).
.TP 8
IFIRST  (input) INTEGER
The index of the first eigenvalue to be computed.
.TP 8
ILAST   (input) INTEGER
The index of the last eigenvalue to be computed.
.TP 8
RTOL1   (input) REAL            
RTOL2   (input) REAL            
Tolerance for the convergence of the bisection intervals.
An interval [LEFT,RIGHT] has converged if
RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )
where GAP is the (estimated) distance to the nearest
eigenvalue.
.TP 8
OFFSET  (input) INTEGER
Offset for the arrays W, WGAP and WERR, i.e., the IFIRST-OFFSET
through ILAST-OFFSET elements of these arrays are to be used.
.TP 8
W       (input/output) REAL             array, dimension (N)
On input, W( IFIRST-OFFSET ) through W( ILAST-OFFSET ) are
estimates of the eigenvalues of L D L^T indexed IFIRST throug
ILAST.
On output, these estimates are refined.
.TP 8
WGAP    (input/output) REAL             array, dimension (N-1)
On input, the (estimated) gaps between consecutive
eigenvalues of L D L^T, i.e., WGAP(I-OFFSET) is the gap between
eigenvalues I and I+1. Note that if IFIRST.EQ.ILAST
then WGAP(IFIRST-OFFSET) must be set to ZERO.
On output, these gaps are refined.
.TP 8
WERR    (input/output) REAL             array, dimension (N)
On input, WERR( IFIRST-OFFSET ) through WERR( ILAST-OFFSET ) are
the errors in the estimates of the corresponding elements in W.
On output, these errors are refined.
.TP 8
WORK    (workspace) REAL             array, dimension (2*N)
Workspace.
.TP 8
IWORK   (workspace) INTEGER array, dimension (2*N)
Workspace.
.TP 8
PIVMIN  (input) DOUBLE PRECISION
The minimum pivot in the Sturm sequence.
.TP 8
SPDIAM  (input) DOUBLE PRECISION
The spectral diameter of the matrix.
.TP 8
TWIST   (input) INTEGER
The twist index for the twisted factorization that is used
for the negcount.
TWIST = N: Compute negcount from L D L^T - LAMBDA I = L+ D+ L+^T
.br
TWIST = 1: Compute negcount from L D L^T - LAMBDA I = U- D- U-^T
.br
TWIST = R: Compute negcount from L D L^T - LAMBDA I = N(r) D(r) N(r)
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

