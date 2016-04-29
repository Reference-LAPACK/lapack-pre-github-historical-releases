.TH  DLARRV 1 "February 2007" " LAPACK auxiliary routine (version 3.1.1) " " LAPACK auxiliary routine (version 3.1.1) " 
.SH NAME
DLARRV - the eigenvectors of the tridiagonal matrix T = L D L^T given L, D and APPROXIMATIONS to the eigenvalues of L D L^T
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARRV(
N, VL, VU, D, L, PIVMIN,
ISPLIT, M, DOL, DOU, MINRGP,
RTOL1, RTOL2, W, WERR, WGAP,
IBLOCK, INDEXW, GERS, Z, LDZ, ISUPPZ,
WORK, IWORK, INFO )
.TP 19
.ti +4
INTEGER
DOL, DOU, INFO, LDZ, M, N
.TP 19
.ti +4
DOUBLE
PRECISION MINRGP, PIVMIN, RTOL1, RTOL2, VL, VU
.TP 19
.ti +4
INTEGER
IBLOCK( * ), INDEXW( * ), ISPLIT( * ),
ISUPPZ( * ), IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), GERS( * ), L( * ), W( * ), WERR( * ),
WGAP( * ), WORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION Z( LDZ, * )
.SH PURPOSE
DLARRV computes the eigenvectors of the tridiagonal matrix
T = L D L^T given L, D and APPROXIMATIONS to the eigenvalues of L D L^T.
The input eigenvalues should have been computed by DLARRE.

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
VL      (input) DOUBLE PRECISION
VU      (input) DOUBLE PRECISION
Lower and upper bounds of the interval that contains the desired
eigenvalues. VL < VU. Needed to compute gaps on the left or right
end of the extremal eigenvalues in the desired RANGE.
.TP 8
D       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the N diagonal elements of the diagonal matrix D.
On exit, D may be overwritten.
.TP 8
L       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the (N-1) subdiagonal elements of the unit
bidiagonal matrix L are in elements 1 to N-1 of L
(if the matrix is not splitted.) At the end of each block
is stored the corresponding shift as given by DLARRE.
On exit, L is overwritten.
.TP 8
PIVMIN  (in) DOUBLE PRECISION
The minimum pivot allowed in the Sturm sequence.
.TP 8
ISPLIT  (input) INTEGER array, dimension (N)
The splitting points, at which T breaks up into blocks.
The first block consists of rows/columns 1 to
ISPLIT( 1 ), the second of rows/columns ISPLIT( 1 )+1
through ISPLIT( 2 ), etc.
.TP 8
M       (input) INTEGER
The total number of input eigenvalues.  0 <= M <= N.
.TP 8
DOL     (input) INTEGER
DOU     (input) INTEGER
If the user wants to compute only selected eigenvectors from all
the eigenvalues supplied, he can specify an index range DOL:DOU.
Or else the setting DOL=1, DOU=M should be applied.
Note that DOL and DOU refer to the order in which the eigenvalues
are stored in W.
If the user wants to compute only selected eigenpairs, then
the columns DOL-1 to DOU+1 of the eigenvector space Z contain the
computed eigenvectors. All other columns of Z are set to zero.
.TP 8
MINRGP  (input) DOUBLE PRECISION
.TP 8
RTOL1   (input) DOUBLE PRECISION
RTOL2   (input) DOUBLE PRECISION
Parameters for bisection.
RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )
.TP 8
W       (input/output) DOUBLE PRECISION array, dimension (N)
The first M elements of W contain the APPROXIMATE eigenvalues for
which eigenvectors are to be computed.  The eigenvalues
should be grouped by split-off block and ordered from
smallest to largest within the block ( The output array
W from DLARRE is expected here ). Furthermore, they are with
respect to the shift of the corresponding root representation
for their block. On exit, W holds the eigenvalues of the
UNshifted matrix.
.TP 8
WERR    (input/output) DOUBLE PRECISION array, dimension (N)
The first M elements contain the semiwidth of the uncertainty
interval of the corresponding eigenvalue in W
.TP 8
WGAP    (input/output) DOUBLE PRECISION array, dimension (N)
The separation from the right neighbor eigenvalue in W.
.TP 8
IBLOCK  (input) INTEGER array, dimension (N)
The indices of the blocks (submatrices) associated with the
corresponding eigenvalues in W; IBLOCK(i)=1 if eigenvalue
W(i) belongs to the first block from the top, =2 if W(i)
belongs to the second block, etc.
.TP 8
INDEXW  (input) INTEGER array, dimension (N)
The indices of the eigenvalues within each block (submatrix);
for example, INDEXW(i)= 10 and IBLOCK(i)=2 imply that the
i-th eigenvalue W(i) is the 10-th eigenvalue in the second block.
.TP 8
GERS    (input) DOUBLE PRECISION array, dimension (2*N)
The N Gerschgorin intervals (the i-th Gerschgorin interval
is (GERS(2*i-1), GERS(2*i)). The Gerschgorin intervals should
be computed from the original UNshifted matrix.
.TP 8
Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )
If INFO = 0, the first M columns of Z contain the
orthonormal eigenvectors of the matrix T
corresponding to the input eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
ISUPPZ  (output) INTEGER array, dimension ( 2*max(1,M) )
The support of the eigenvectors in Z, i.e., the indices
indicating the nonzero elements in Z. The I-th eigenvector
is nonzero only in elements ISUPPZ( 2*I-1 ) through
ISUPPZ( 2*I ).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (12*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (7*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit

> 0:  A problem occured in DLARRV.
.br
< 0:  One of the called subroutines signaled an internal problem.
Needs inspection of the corresponding parameter IINFO
for further information.
.TP 6
=-1:  Problem in DLARRB when refining a child\(aqs eigenvalues.
=-2:  Problem in DLARRF when computing the RRR of a child.
When a child is inside a tight cluster, it can be difficult
to find an RRR. A partial remedy from the user\(aqs point of
view is to make the parameter MINRGP smaller and recompile.
However, as the orthogonality of the computed vectors is
proportional to 1/MINRGP, the user should be aware that
he might be trading in precision when he decreases MINRGP.
=-3:  Problem in DLARRB when refining a single eigenvalue
after the Rayleigh correction was rejected.
= 5:  The Rayleigh Quotient Iteration failed to converge to
full accuracy in MAXITR steps.
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

