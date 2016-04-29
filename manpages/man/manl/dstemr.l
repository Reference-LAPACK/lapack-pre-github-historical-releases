.TH  DSTEMR 1 "November 2006" " LAPACK computational routine (version 3.1) " " LAPACK computational routine (version 3.1) " 
.SH NAME
DSTEMR - selected eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix T
.SH SYNOPSIS
.TP 19
SUBROUTINE DSTEMR(
JOBZ, RANGE, N, D, E, VL, VU, IL, IU,
M, W, Z, LDZ, NZC, ISUPPZ, TRYRAC, WORK, LWORK,
IWORK, LIWORK, INFO )
.TP 19
.ti +4
IMPLICIT
NONE
.TP 19
.ti +4
CHARACTER
JOBZ, RANGE
.TP 19
.ti +4
LOGICAL
TRYRAC
.TP 19
.ti +4
INTEGER
IL, INFO, IU, LDZ, NZC, LIWORK, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION VL, VU
.TP 19
.ti +4
INTEGER
ISUPPZ( * ), IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), W( * ), WORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION Z( LDZ, * )
.SH PURPOSE
DSTEMR computes selected eigenvalues and, optionally, eigenvectors
of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
a well defined set of pairwise different real eigenvalues, the corresponding
real eigenvectors are pairwise orthogonal.
.br

The spectrum may be computed either completely or partially by specifying
either an interval (VL,VU] or a range of indices IL:IU for the desired
eigenvalues.
.br

Depending on the number of desired eigenvalues, these are computed either
by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
computed by the use of various suitable L D L^T factorizations near clusters
of close eigenvalues (referred to as RRRs, Relatively Robust
Representations). An informal sketch of the algorithm follows.

For each unreduced block (submatrix) of T,
.br
   (a) Compute T - sigma I  = L D L^T, so that L and D
.br
       define all the wanted eigenvalues to high relative accuracy.
       This means that small relative changes in the entries of D and L
       cause only small relative changes in the eigenvalues and
       eigenvectors. The standard (unfactored) representation of the
       tridiagonal matrix T does not have this property in general.
   (b) Compute the eigenvalues to suitable accuracy.
.br
       If the eigenvectors are desired, the algorithm attains full
       accuracy of the computed eigenvalues only right before
       the corresponding vectors have to be computed, see steps c) and d).
   (c) For each cluster of close eigenvalues, select a new
       shift close to the cluster, find a new factorization, and refine
       the shifted eigenvalues to suitable accuracy.
.br
   (d) For each eigenvalue with a large enough relative separation compute
       the corresponding eigenvector by forming a rank revealing twisted
       factorization. Go back to (c) for any clusters that remain.

For more details, see:
.br
- Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
  to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
  Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
- Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
  Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
  2004.  Also LAPACK Working Note 154.
.br
- Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
  tridiagonal eigenvalue/eigenvector problem",
.br
  Computer Science Division Technical Report No. UCB/CSD-97-971,
  UC Berkeley, May 1997.
.br

Notes:
.br
1.DSTEMR works only on machines which follow IEEE-754
.br
floating-point standard in their handling of infinities and NaNs.
This permits the use of efficient inner loops avoiding a check for
zero divisors.
.br

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
= \(aqN\(aq:  Compute eigenvalues only;
.br
= \(aqV\(aq:  Compute eigenvalues and eigenvectors.
.TP 8
RANGE   (input) CHARACTER*1
.br
= \(aqA\(aq: all eigenvalues will be found.
.br
= \(aqV\(aq: all eigenvalues in the half-open interval (VL,VU]
will be found.
= \(aqI\(aq: the IL-th through IU-th eigenvalues will be found.
.TP 8
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the N diagonal elements of the tridiagonal matrix
T. On exit, D is overwritten.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the (N-1) subdiagonal elements of the tridiagonal
matrix T in elements 1 to N-1 of E. E(N) need not be set on
input, but is used internally as workspace.
On exit, E is overwritten.
.TP 8
VL      (input) DOUBLE PRECISION
VU      (input) DOUBLE PRECISION
If RANGE=\(aqV\(aq, the lower and upper bounds of the interval to
be searched for eigenvalues. VL < VU.
Not referenced if RANGE = \(aqA\(aq or \(aqI\(aq.
.TP 8
IL      (input) INTEGER
IU      (input) INTEGER
If RANGE=\(aqI\(aq, the indices (in ascending order) of the
smallest and largest eigenvalues to be returned.
1 <= IL <= IU <= N, if N > 0.
Not referenced if RANGE = \(aqA\(aq or \(aqV\(aq.
.TP 8
M       (output) INTEGER
The total number of eigenvalues found.  0 <= M <= N.
If RANGE = \(aqA\(aq, M = N, and if RANGE = \(aqI\(aq, M = IU-IL+1.
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
The first M elements contain the selected eigenvalues in
ascending order.
.TP 8
Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )
If JOBZ = \(aqV\(aq, and if INFO = 0, then the first M columns of Z
contain the orthonormal eigenvectors of the matrix T
corresponding to the selected eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
If JOBZ = \(aqN\(aq, then Z is not referenced.
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z; if RANGE = \(aqV\(aq, the exact value of M
is not known in advance and can be computed with a workspace
query by setting NZC = -1, see below.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, then LDZ >= max(1,N).
.TP 8
NZC     (input) INTEGER
The number of eigenvectors to be held in the array Z.
If RANGE = \(aqA\(aq, then NZC >= max(1,N).
If RANGE = \(aqV\(aq, then NZC >= the number of eigenvalues in (VL,VU].
If RANGE = \(aqI\(aq, then NZC >= IU-IL+1.
If NZC = -1, then a workspace query is assumed; the
routine calculates the number of columns of the array Z that
are needed to hold the eigenvectors.
This value is returned as the first entry of the Z array, and
no error message related to NZC is issued by XERBLA.
.TP 8
ISUPPZ  (output) INTEGER ARRAY, dimension ( 2*max(1,M) )
The support of the eigenvectors in Z, i.e., the indices
indicating the nonzero elements in Z. The i-th computed eigenvector
is nonzero only in elements ISUPPZ( 2*i-1 ) through
ISUPPZ( 2*i ). This is relevant in the case when the matrix
is split. ISUPPZ is only accessed when JOBZ is \(aqV\(aq and N > 0.
.TP 8
TRYRAC  (input/output) LOGICAL
If TRYRAC.EQ..TRUE., indicates that the code should check whether
the tridiagonal matrix defines its eigenvalues to high relative
accuracy.  If so, the code uses relative-accuracy preserving
algorithms that might be (a bit) slower depending on the matrix.
If the matrix does not define its eigenvalues to high relative
accuracy, the code can uses possibly faster algorithms.
If TRYRAC.EQ..FALSE., the code is not required to guarantee
relatively accurate eigenvalues and can use the fastest possible
techniques.
On exit, a .TRUE. TRYRAC will be set to .FALSE. if the matrix
does not define its eigenvalues to high relative accuracy.
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal
(and minimal) LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,18*N)
if JOBZ = \(aqV\(aq, and LWORK >= max(1,12*N) if JOBZ = \(aqN\(aq.
If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (LIWORK)
On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK.  LIWORK >= max(1,10*N)
if the eigenvectors are desired, and LIWORK >= max(1,8*N)
if only the eigenvalues are to be computed.
If LIWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal size of the IWORK array,
returns this value as the first entry of the IWORK array, and
no error message related to LIWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
On exit, INFO
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = 1X, internal error in DLARRE,
if INFO = 2X, internal error in DLARRV.
Here, the digit X = ABS( IINFO ) < 10, where IINFO is
the nonzero error code returned by DLARRE or
DLARRV, respectively.
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

