.TH  SLARRE 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLARRE - the desired eigenvalues of a given real symmetric tridiagonal matrix T, SLARRE sets any "small" off-diagonal elements to zero, and for each unreduced block T_i, it finds (a) a suitable shift at one end of the block\(aqs spectrum,
.SH SYNOPSIS
.TP 19
SUBROUTINE SLARRE(
RANGE, N, VL, VU, IL, IU, D, E, E2,
RTOL1, RTOL2, SPLTOL, NSPLIT, ISPLIT, M,
W, WERR, WGAP, IBLOCK, INDEXW, GERS, PIVMIN,
WORK, IWORK, INFO )
.TP 19
.ti +4
IMPLICIT
NONE
.TP 19
.ti +4
CHARACTER
RANGE
.TP 19
.ti +4
INTEGER
IL, INFO, IU, M, N, NSPLIT
.TP 19
.ti +4
REAL
PIVMIN, RTOL1, RTOL2, SPLTOL, VL, VU
.TP 19
.ti +4
INTEGER
IBLOCK( * ), ISPLIT( * ), IWORK( * ),
INDEXW( * )
.TP 19
.ti +4
REAL
D( * ), E( * ), E2( * ), GERS( * ),
W( * ),WERR( * ), WGAP( * ), WORK( * )
.SH PURPOSE
To find the desired eigenvalues of a given real symmetric
tridiagonal matrix T, SLARRE sets any "small" off-diagonal
elements to zero, and for each unreduced block T_i, it finds
(a) a suitable shift at one end of the block\(aqs spectrum,
(b) the base representation, T_i - sigma_i I = L_i D_i L_i^T, and
(c) eigenvalues of each L_i D_i L_i^T.
.br
The representations and eigenvalues found are then used by
SSTEMR to compute the eigenvectors of T.
.br
The accuracy varies depending on whether bisection is used to
find a few eigenvalues or the dqds algorithm (subroutine SLASQ2) to
conpute all and then discard any unwanted one.
.br
As an added benefit, SLARRE also outputs the n
.br
Gerschgorin intervals for the matrices L_i D_i L_i^T.
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
N       (input) INTEGER
The order of the matrix. N > 0.
.TP 8
VL      (input/output) REAL            
VU      (input/output) REAL            
If RANGE=\(aqV\(aq, the lower and upper bounds for the eigenvalues.
Eigenvalues less than or equal to VL, or greater than VU,
will not be returned.  VL < VU.
If RANGE=\(aqI\(aq or =\(aqA\(aq, SLARRE computes bounds on the desired
part of the spectrum.
.TP 8
IL      (input) INTEGER
IU      (input) INTEGER
If RANGE=\(aqI\(aq, the indices (in ascending order) of the
smallest and largest eigenvalues to be returned.
1 <= IL <= IU <= N.
.TP 8
D       (input/output) REAL             array, dimension (N)
On entry, the N diagonal elements of the tridiagonal
matrix T.
On exit, the N diagonal elements of the diagonal
matrices D_i.
.TP 8
E       (input/output) REAL             array, dimension (N)
On entry, the first (N-1) entries contain the subdiagonal
elements of the tridiagonal matrix T; E(N) need not be set.
On exit, E contains the subdiagonal elements of the unit
bidiagonal matrices L_i. The entries E( ISPLIT( I ) ),
1 <= I <= NSPLIT, contain the base points sigma_i on output.
.TP 8
E2      (input/output) REAL             array, dimension (N)
On entry, the first (N-1) entries contain the SQUARES of the
subdiagonal elements of the tridiagonal matrix T;
E2(N) need not be set.
On exit, the entries E2( ISPLIT( I ) ),
1 <= I <= NSPLIT, have been set to zero
.TP 8
RTOL1   (input) REAL            
RTOL2   (input) REAL            
Parameters for bisection.
RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )

SPLTOL (input) REAL            
The threshold for splitting.
.TP 8
NSPLIT  (output) INTEGER
The number of blocks T splits into. 1 <= NSPLIT <= N.
.TP 8
ISPLIT  (output) INTEGER array, dimension (N)
The splitting points, at which T breaks up into blocks.
The first block consists of rows/columns 1 to ISPLIT(1),
the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
etc., and the NSPLIT-th consists of rows/columns
ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
.TP 8
M       (output) INTEGER
The total number of eigenvalues (of all L_i D_i L_i^T)
found.
.TP 8
W       (output) REAL             array, dimension (N)
The first M elements contain the eigenvalues. The
eigenvalues of each of the blocks, L_i D_i L_i^T, are
sorted in ascending order ( SLARRE may use the
remaining N-M elements as workspace).
.TP 8
WERR    (output) REAL             array, dimension (N)
The error bound on the corresponding eigenvalue in W.
.TP 8
WGAP    (output) REAL             array, dimension (N)
The separation from the right neighbor eigenvalue in W.
The gap is only with respect to the eigenvalues of the same block
as each block has its own representation tree.
Exception: at the right end of a block we store the left gap
.TP 8
IBLOCK  (output) INTEGER array, dimension (N)
The indices of the blocks (submatrices) associated with the
corresponding eigenvalues in W; IBLOCK(i)=1 if eigenvalue
W(i) belongs to the first block from the top, =2 if W(i)
belongs to the second block, etc.
.TP 8
INDEXW  (output) INTEGER array, dimension (N)
The indices of the eigenvalues within each block (submatrix);
for example, INDEXW(i)= 10 and IBLOCK(i)=2 imply that the
i-th eigenvalue W(i) is the 10-th eigenvalue in block 2
.TP 8
GERS    (output) REAL             array, dimension (2*N)
The N Gerschgorin intervals (the i-th Gerschgorin interval
is (GERS(2*i-1), GERS(2*i)).
.TP 8
PIVMIN  (output) DOUBLE PRECISION
The minimum pivot in the Sturm sequence for T.
.TP 8
WORK    (workspace) REAL             array, dimension (6*N)
Workspace.
.TP 8
IWORK   (workspace) INTEGER array, dimension (5*N)
Workspace.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
> 0:  A problem occured in SLARRE.
.br
< 0:  One of the called subroutines signaled an internal problem.
Needs inspection of the corresponding parameter IINFO
for further information.
.TP 6
=-1:  Problem in SLARRD.
= 2:  No base representation could be found in MAXTRY iterations.
Increasing MAXTRY and recompilation might be a remedy.
=-3:  Problem in SLARRB when computing the refined root
representation for SLASQ2.
=-4:  Problem in SLARRB when preforming bisection on the
desired part of the spectrum.
=-5:  Problem in SLASQ2.
.br
=-6:  Problem in SLASQ2.

Further Details
element growth and consequently define all their eigenvalues to
high relative accuracy.
===============

Based on contributions by
Beresford Parlett, University of California, Berkeley, USA
Jim Demmel, University of California, Berkeley, USA
Inderjit Dhillon, University of Texas, Austin, USA
Osni Marques, LBNL/NERSC, USA
Christof Voemel, University of California, Berkeley, USA
