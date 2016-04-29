.TH  CSTEGR 1 "November 2006" " LAPACK computational routine (version 3.1) " " LAPACK computational routine (version 3.1) " 
.SH NAME
CSTEGR - selected eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix T
.SH SYNOPSIS
.TP 19
SUBROUTINE CSTEGR(
JOBZ, RANGE, N, D, E, VL, VU, IL, IU,
ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,
LIWORK, INFO )
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
INTEGER
IL, INFO, IU, LDZ, LIWORK, LWORK, M, N
.TP 19
.ti +4
REAL
ABSTOL, VL, VU
.TP 19
.ti +4
INTEGER
ISUPPZ( * ), IWORK( * )
.TP 19
.ti +4
REAL
D( * ), E( * ), W( * ), WORK( * )
.TP 19
.ti +4
COMPLEX
Z( LDZ, * )
.SH PURPOSE
CSTEGR computes selected eigenvalues and, optionally, eigenvectors
of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
a well defined set of pairwise different real eigenvalues, the corresponding
real eigenvectors are pairwise orthogonal.
.br

The spectrum may be computed either completely or partially by specifying
either an interval (VL,VU] or a range of indices IL:IU for the desired
eigenvalues.
.br

CSTEGR is a compatability wrapper around the improved CSTEMR routine.
See SSTEMR for further details.
.br

One important change is that the ABSTOL parameter no longer provides any
benefit and hence is no longer used.
.br

Note : CSTEGR and CSTEMR work only on machines which follow
IEEE-754 floating-point standard in their handling of infinities and
NaNs.  Normal execution may create these exceptiona values and hence
may abort due to a floating point exception in environments which
do not conform to the IEEE-754 standard.
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
D       (input/output) REAL array, dimension (N)
On entry, the N diagonal elements of the tridiagonal matrix
T. On exit, D is overwritten.
.TP 8
E       (input/output) REAL array, dimension (N)
On entry, the (N-1) subdiagonal elements of the tridiagonal
matrix T in elements 1 to N-1 of E. E(N) need not be set on
input, but is used internally as workspace.
On exit, E is overwritten.
.TP 8
VL      (input) REAL
VU      (input) REAL
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
ABSTOL  (input) REAL
Unused.  Was the absolute error tolerance for the
eigenvalues/eigenvectors in previous versions.
.TP 8
M       (output) INTEGER
The total number of eigenvalues found.  0 <= M <= N.
If RANGE = \(aqA\(aq, M = N, and if RANGE = \(aqI\(aq, M = IU-IL+1.
.TP 8
W       (output) REAL array, dimension (N)
The first M elements contain the selected eigenvalues in
ascending order.
.TP 8
Z       (output) COMPLEX array, dimension (LDZ, max(1,M) )
If JOBZ = \(aqV\(aq, and if INFO = 0, then the first M columns of Z
contain the orthonormal eigenvectors of the matrix T
corresponding to the selected eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
If JOBZ = \(aqN\(aq, then Z is not referenced.
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z; if RANGE = \(aqV\(aq, the exact value of M
is not known in advance and an upper bound must be used.
Supplying N columns is always safe.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, then LDZ >= max(1,N).
.TP 8
ISUPPZ  (output) INTEGER ARRAY, dimension ( 2*max(1,M) )
The support of the eigenvectors in Z, i.e., the indices
indicating the nonzero elements in Z. The i-th computed eigenvector
is nonzero only in elements ISUPPZ( 2*i-1 ) through
ISUPPZ( 2*i ). This is relevant in the case when the matrix
is split. ISUPPZ is only accessed when JOBZ is \(aqV\(aq and N > 0.
.TP 8
WORK    (workspace/output) REAL array, dimension (LWORK)
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
> 0:  if INFO = 1X, internal error in SLARRE,
if INFO = 2X, internal error in CLARRV.
Here, the digit X = ABS( IINFO ) < 10, where IINFO is
the nonzero error code returned by SLARRE or
CLARRV, respectively.
.SH FURTHER DETAILS
Based on contributions by
.br
   Inderjit Dhillon, IBM Almaden, USA
.br
   Osni Marques, LBNL/NERSC, USA
.br
   Christof Voemel, LBNL/NERSC, USA
.br

