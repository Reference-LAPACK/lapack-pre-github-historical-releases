.TH  DLARRD 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARRD - the eigenvalues of a symmetric tridiagonal matrix T to suitable accuracy
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARRD(
RANGE, ORDER, N, VL, VU, IL, IU, GERS,
RELTOL, D, E, E2, PIVMIN, NSPLIT, ISPLIT,
M, W, WERR, WL, WU, IBLOCK, INDEXW,
WORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
ORDER, RANGE
.TP 19
.ti +4
INTEGER
IL, INFO, IU, M, N, NSPLIT
.TP 19
.ti +4
DOUBLE
PRECISION PIVMIN, RELTOL, VL, VU, WL, WU
.TP 19
.ti +4
INTEGER
IBLOCK( * ), INDEXW( * ),
ISPLIT( * ), IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), E2( * ),
GERS( * ), W( * ), WERR( * ), WORK( * )
.SH PURPOSE
DLARRD computes the eigenvalues of a symmetric tridiagonal
matrix T to suitable accuracy. This is an auxiliary code to be
called from DSTEMR.
.br
The user may ask for all eigenvalues, all eigenvalues
.br
in the half-open interval (VL, VU], or the IL-th through IU-th
eigenvalues.
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
RANGE   (input) CHARACTER
= \(aqA\(aq: ("All")   all eigenvalues will be found.
.br
= \(aqV\(aq: ("Value") all eigenvalues in the half-open interval
(VL, VU] will be found.
= \(aqI\(aq: ("Index") the IL-th through IU-th eigenvalues (of the
entire matrix) will be found.
.TP 8
ORDER   (input) CHARACTER
= \(aqB\(aq: ("By Block") the eigenvalues will be grouped by
split-off block (see IBLOCK, ISPLIT) and
ordered from smallest to largest within
the block.
= \(aqE\(aq: ("Entire matrix")
the eigenvalues for the entire matrix
will be ordered from smallest to
largest.
.TP 8
N       (input) INTEGER
The order of the tridiagonal matrix T.  N >= 0.
.TP 8
VL      (input) DOUBLE PRECISION
VU      (input) DOUBLE PRECISION
If RANGE=\(aqV\(aq, the lower and upper bounds of the interval to
be searched for eigenvalues.  Eigenvalues less than or equal
to VL, or greater than VU, will not be returned.  VL < VU.
Not referenced if RANGE = \(aqA\(aq or \(aqI\(aq.
.TP 8
IL      (input) INTEGER
IU      (input) INTEGER
If RANGE=\(aqI\(aq, the indices (in ascending order) of the
smallest and largest eigenvalues to be returned.
1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
Not referenced if RANGE = \(aqA\(aq or \(aqV\(aq.
.TP 8
GERS    (input) DOUBLE PRECISION array, dimension (2*N)
The N Gerschgorin intervals (the i-th Gerschgorin interval
is (GERS(2*i-1), GERS(2*i)).
.TP 8
RELTOL  (input) DOUBLE PRECISION
The minimum relative width of an interval.  When an interval
is narrower than RELTOL times the larger (in
magnitude) endpoint, then it is considered to be
sufficiently small, i.e., converged.  Note: this should
always be at least radix*machine epsilon.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The n diagonal elements of the tridiagonal matrix T.
.TP 8
E       (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) off-diagonal elements of the tridiagonal matrix T.
.TP 8
E2      (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) squared off-diagonal elements of the tridiagonal matrix T.
.TP 8
PIVMIN  (input) DOUBLE PRECISION
The minimum pivot allowed in the Sturm sequence for T.
.TP 8
NSPLIT  (input) INTEGER
The number of diagonal blocks in the matrix T.
1 <= NSPLIT <= N.
.TP 8
ISPLIT  (input) INTEGER array, dimension (N)
The splitting points, at which T breaks up into submatrices.
The first submatrix consists of rows/columns 1 to ISPLIT(1),
the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
etc., and the NSPLIT-th consists of rows/columns
ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
(Only the first NSPLIT elements will actually be used, but
since the user cannot know a priori what value NSPLIT will
have, N words must be reserved for ISPLIT.)
.TP 8
M       (output) INTEGER
The actual number of eigenvalues found. 0 <= M <= N.
(See also the description of INFO=2,3.)
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
On exit, the first M elements of W will contain the
eigenvalue approximations. DLARRD computes an interval
I_j = (a_j, b_j] that includes eigenvalue j. The eigenvalue
approximation is given as the interval midpoint
W(j)= ( a_j + b_j)/2. The corresponding error is bounded by
WERR(j) = abs( a_j - b_j)/2
.TP 8
WERR    (output) DOUBLE PRECISION array, dimension (N)
The error bound on the corresponding eigenvalue approximation
in W.
.TP 8
WL      (output) DOUBLE PRECISION
WU      (output) DOUBLE PRECISION
The interval (WL, WU] contains all the wanted eigenvalues.
If RANGE=\(aqV\(aq, then WL=VL and WU=VU.
If RANGE=\(aqA\(aq, then WL and WU are the global Gerschgorin bounds
on the spectrum.
If RANGE=\(aqI\(aq, then WL and WU are computed by DLAEBZ from the
index range specified.
.TP 8
IBLOCK  (output) INTEGER array, dimension (N)
At each row/column j where E(j) is zero or small, the
matrix T is considered to split into a block diagonal
matrix.  On exit, if INFO = 0, IBLOCK(i) specifies to which
block (from 1 to the number of blocks) the eigenvalue W(i)
belongs.  (DLARRD may use the remaining N-M elements as
workspace.)
.TP 8
INDEXW  (output) INTEGER array, dimension (N)
The indices of the eigenvalues within each block (submatrix);
for example, INDEXW(i)= j and IBLOCK(i)=k imply that the
i-th eigenvalue W(i) is the j-th eigenvalue in block k.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (3*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  some or all of the eigenvalues failed to converge or
.br
were not computed:
.br
=1 or 3: Bisection failed to converge for some
eigenvalues; these eigenvalues are flagged by a
negative block number.  The effect is that the
eigenvalues may not be as accurate as the
absolute and relative tolerances.  This is
generally caused by unexpectedly inaccurate
arithmetic.
=2 or 3: RANGE=\(aqI\(aq only: Not all of the eigenvalues
.br
IL:IU were found.
.br
Effect: M < IU+1-IL
.br
Cause:  non-monotonic arithmetic, causing the
Sturm sequence to be non-monotonic.
Cure:   recalculate, using RANGE=\(aqA\(aq, and pick
.br
out eigenvalues IL:IU.  In some cases,
increasing the PARAMETER "FUDGE" may
make things work.
= 4:    RANGE=\(aqI\(aq, and the Gershgorin interval
initially used was too small.  No eigenvalues
were computed.
Probable cause: your machine has sloppy
floating-point arithmetic.
Cure: Increase the PARAMETER "FUDGE",
recompile, and try again.
.SH PARAMETERS
.TP 8
FUDGE   DOUBLE PRECISION, default = 2
A "fudge factor" to widen the Gershgorin intervals.  Ideally,
a value of 1 should work, but on machines with sloppy
arithmetic, this needs to be larger.  The default for
publicly released versions should be large enough to handle
the worst machine around.  Note that this has no effect
on accuracy of the solution.

Based on contributions by
W. Kahan, University of California, Berkeley, USA
Beresford Parlett, University of California, Berkeley, USA
Jim Demmel, University of California, Berkeley, USA
Inderjit Dhillon, University of Texas, Austin, USA
Osni Marques, LBNL/NERSC, USA
Christof Voemel, University of California, Berkeley, USA
