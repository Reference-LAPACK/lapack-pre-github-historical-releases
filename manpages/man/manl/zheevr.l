.TH  ZHEEVR 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZHEEVR - selected eigenvalues and, optionally, eigenvectors of a complex Hermitian matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE ZHEEVR(
JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU,
ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,
RWORK, LRWORK, IWORK, LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, RANGE, UPLO
.TP 19
.ti +4
INTEGER
IL, INFO, IU, LDA, LDZ, LIWORK, LRWORK, LWORK,
M, N
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
PRECISION RWORK( * ), W( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
ZHEEVR computes selected eigenvalues and, optionally, eigenvectors
of a complex Hermitian matrix A.  Eigenvalues and eigenvectors can
be selected by specifying either a range of values or a range of
indices for the desired eigenvalues.
.br

ZHEEVR first reduces the matrix A to tridiagonal form T with a call
to ZHETRD.  Then, whenever possible, ZHEEVR calls ZSTEMR to compute
eigenspectrum using Relatively Robust Representations.  ZSTEMR
computes eigenvalues by the dqds algorithm, while orthogonal
eigenvectors are computed from various "good" L D L^T representations
(also known as Relatively Robust Representations). Gram-Schmidt
orthogonalization is avoided as far as possible. More specifically,
the various steps of the algorithm are as follows.
.br

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

The desired accuracy of the output can be specified by the input
parameter ABSTOL.
.br

For more details, see DSTEMR\(aqs documentation and:
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


Note 1 : ZHEEVR calls ZSTEMR when the full spectrum is requested
on machines which conform to the ieee-754 floating point standard.
ZHEEVR calls DSTEBZ and ZSTEIN on non-ieee machines and
.br
when partial spectrum requests are made.
.br

Normal execution of ZSTEMR may create NaNs and infinities and
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
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA, N)
On entry, the Hermitian matrix A.  If UPLO = \(aqU\(aq, the
leading N-by-N upper triangular part of A contains the
upper triangular part of the matrix A.  If UPLO = \(aqL\(aq,
the leading N-by-N lower triangular part of A contains
the lower triangular part of the matrix A.
On exit, the lower triangle (if UPLO=\(aqL\(aq) or the upper
triangle (if UPLO=\(aqU\(aq) of A, including the diagonal, is
destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
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
furutre releases will. See J. Barlow and J. Demmel,
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
Z       (output) COMPLEX*16 array, dimension (LDZ, max(1,M))
If JOBZ = \(aqV\(aq, then if INFO = 0, the first M columns of Z
contain the orthonormal eigenvectors of the matrix A
corresponding to the selected eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
If JOBZ = \(aqN\(aq, then Z is not referenced.
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
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of the array WORK.  LWORK >= max(1,2*N).
For optimal efficiency, LWORK >= (NB+1)*N,
where NB is the max of the blocksize for ZHETRD and for
ZUNMTR as returned by ILAENV.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal sizes of the WORK, RWORK and
IWORK arrays, returns these values as the first entries of
the WORK, RWORK and IWORK arrays, and no error message
related to LWORK or LRWORK or LIWORK is issued by XERBLA.
.TP 8
RWORK   (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LRWORK))
On exit, if INFO = 0, RWORK(1) returns the optimal
(and minimal) LRWORK.

The length of the array RWORK.  LRWORK >= max(1,24*N).

If LRWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal sizes of the WORK, RWORK
and IWORK arrays, returns these values as the first entries
of the WORK, RWORK and IWORK arrays, and no error message
related to LWORK or LRWORK or LIWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
On exit, if INFO = 0, IWORK(1) returns the optimal
(and minimal) LIWORK.

The dimension of the array IWORK.  LIWORK >= max(1,10*N).

If LIWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal sizes of the WORK, RWORK
and IWORK arrays, returns these values as the first entries
of the WORK, RWORK and IWORK arrays, and no error message
related to LWORK or LRWORK or LIWORK is issued by XERBLA.
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
   Jason Riedy, Computer Science Division, University of
.br
     California at Berkeley, USA
.br

