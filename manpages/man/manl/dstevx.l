.TH  DSTEVX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DSTEVX - selected eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE DSTEVX(
JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL,
M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, RANGE
.TP 19
.ti +4
INTEGER
IL, INFO, IU, LDZ, M, N
.TP 19
.ti +4
DOUBLE
PRECISION ABSTOL, VL, VU
.TP 19
.ti +4
INTEGER
IFAIL( * ), IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
DSTEVX computes selected eigenvalues and, optionally, eigenvectors
of a real symmetric tridiagonal matrix A.  Eigenvalues and
eigenvectors can be selected by specifying either a range of values
or a range of indices for the desired eigenvalues.
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

where EPS is the machine precision.  If ABSTOL is less
than or equal to zero, then  EPS*|T|  will be used in
its place, where |T| is the 1-norm of the tridiagonal
matrix.

Eigenvalues will be computed most accurately when ABSTOL is
set to twice the underflow threshold 2*DLAMCH(\(aqS\(aq), not zero.
If this routine returns with INFO>0, indicating that some
eigenvectors did not converge, try setting ABSTOL to
2*DLAMCH(\(aqS\(aq).

See "Computing Small Singular Values of Bidiagonal Matrices
with Guaranteed High Relative Accuracy," by Demmel and
Kahan, LAPACK Working Note #3.
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
If an eigenvector fails to converge (INFO > 0), then that
column of Z contains the latest approximation to the
eigenvector, and the index of the eigenvector is returned
in IFAIL.  If JOBZ = \(aqN\(aq, then Z is not referenced.
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z; if RANGE = \(aqV\(aq, the exact value of M
is not known in advance and an upper bound must be used.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (5*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (5*N)
.TP 8
IFAIL   (output) INTEGER array, dimension (N)
If JOBZ = \(aqV\(aq, then if INFO = 0, the first M elements of
IFAIL are zero.  If INFO > 0, then IFAIL contains the
indices of the eigenvectors that failed to converge.
If JOBZ = \(aqN\(aq, then IFAIL is not referenced.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, then i eigenvectors failed to converge.
Their indices are stored in array IFAIL.
