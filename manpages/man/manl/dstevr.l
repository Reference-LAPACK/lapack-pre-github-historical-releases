.TH  DSTEVR 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DSTEVR - selected eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix T
.SH SYNOPSIS
.TP 19
SUBROUTINE DSTEVR(
JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL,
M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,
LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, RANGE
.TP 19
.ti +4
INTEGER
IL, INFO, IU, LDZ, LIWORK, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION ABSTOL, VL, VU
.TP 19
.ti +4
INTEGER
ISUPPZ( * ), IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
DSTEVR computes selected eigenvalues and, optionally, eigenvectors
of a real symmetric tridiagonal matrix T.  Eigenvalues and
eigenvectors can be selected by specifying either a range of values
or a range of indices for the desired eigenvalues.
.br

Whenever possible, DSTEVR calls DSTEMR to compute the
.br
eigenspectrum using Relatively Robust Representations.  DSTEMR
computes eigenvalues by the dqds algorithm, while orthogonal
eigenvectors are computed from various "good" L D L^T representations
(also known as Relatively Robust Representations). Gram-Schmidt
orthogonalization is avoided as far as possible. More specifically,
the various steps of the algorithm are as follows. For the i-th
unreduced block of T,
.br
   (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
        is a relatively robust representation,
.br
   (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
       relative accuracy by the dqds algorithm,
.br
   (c) If there is a cluster of close eigenvalues, "choose" sigma_i
       close to the cluster, and go to step (a),
.br
   (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
       compute the corresponding eigenvector by forming a
       rank-revealing twisted factorization.
.br
The desired accuracy of the output can be specified by the input
parameter ABSTOL.
.br

For more details, see "A new O(n^2) algorithm for the symmetric
tridiagonal eigenvalue/eigenvector problem", by Inderjit Dhillon,
Computer Science Division Technical Report No. UCB//CSD-97-971,
UC Berkeley, May 1997.
.br


Note 1 : DSTEVR calls DSTEMR when the full spectrum is requested
on machines which conform to the ieee-754 floating point standard.
DSTEVR calls DSTEBZ and DSTEIN on non-ieee machines and
.br
when partial spectrum requests are made.
.br

Normal execution of DSTEMR may create NaNs and infinities and
hence may abort due to a floating point exception in environments
which do not handle NaNs and infinities in the ieee standard default
manner.
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
On entry, the n diagonal elements of the tridiagonal matrix
A.
On exit, D may be multiplied by a constant factor chosen
to avoid over/underflow in computing the eigenvalues.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (max(1,N-1))
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix A in elements 1 to N-1 of E.
On exit, E may be multiplied by a constant factor chosen
to avoid over/underflow in computing the eigenvalues.
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
1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
Not referenced if RANGE = \(aqA\(aq or \(aqV\(aq.
.TP 8
ABSTOL  (input) DOUBLE PRECISION
The absolute error tolerance for the eigenvalues.
An approximate eigenvalue is accepted as converged
when it is determined to lie in an interval [a,b]
of width less than or equal to

ABSTOL + EPS *   max( |a|,|b| ) ,

where EPS is the machine precision.  If ABSTOL is less than
or equal to zero, then  EPS*|T|  will be used in its place,
where |T| is the 1-norm of the tridiagonal matrix obtained
by reducing A to tridiagonal form.

See "Computing Small Singular Values of Bidiagonal Matrices
with Guaranteed High Relative Accuracy," by Demmel and
Kahan, LAPACK Working Note #3.

If high relative accuracy is important, set ABSTOL to
DLAMCH( \(aqSafe minimum\(aq ).  Doing so will guarantee that
eigenvalues are computed to high relative accuracy when
possible in future releases.  The current code does not
make any guarantees about high relative accuracy, but
future releases will. See J. Barlow and J. Demmel,
"Computing Accurate Eigensystems of Scaled Diagonally
Dominant Matrices", LAPACK Working Note #7, for a discussion
of which matrices define their eigenvalues to high relative
accuracy.
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
If JOBZ = \(aqV\(aq, then if INFO = 0, the first M columns of Z
contain the orthonormal eigenvectors of the matrix A
corresponding to the selected eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z; if RANGE = \(aqV\(aq, the exact value of M
is not known in advance and an upper bound must be used.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
ISUPPZ  (output) INTEGER array, dimension ( 2*max(1,M) )
The support of the eigenvectors in Z, i.e., the indices
indicating the nonzero elements in Z. The i-th eigenvector
is nonzero only in elements ISUPPZ( 2*i-1 ) through
ISUPPZ( 2*i ).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal (and
minimal) LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,20*N).

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal sizes of the WORK and IWORK
arrays, returns these values as the first entries of the WORK
and IWORK arrays, and no error message related to LWORK or
LIWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
On exit, if INFO = 0, IWORK(1) returns the optimal (and
minimal) LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK.  LIWORK >= max(1,10*N).

If LIWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal sizes of the WORK and
IWORK arrays, returns these values as the first entries of
the WORK and IWORK arrays, and no error message related to
LWORK or LIWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  Internal error
.SH FURTHER DETAILS
Based on contributions by
.br
   Inderjit Dhillon, IBM Almaden, USA
.br
   Osni Marques, LBNL/NERSC, USA
.br
   Ken Stanley, Computer Science Division, University of
.br
     California at Berkeley, USA
.br

