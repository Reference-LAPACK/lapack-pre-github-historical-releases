.TH  CHBEVX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CHBEVX - selected eigenvalues and, optionally, eigenvectors of a complex Hermitian band matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CHBEVX(
JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ, VL,
VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK,
IWORK, IFAIL, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, RANGE, UPLO
.TP 19
.ti +4
INTEGER
IL, INFO, IU, KD, LDAB, LDQ, LDZ, M, N
.TP 19
.ti +4
REAL
ABSTOL, VL, VU
.TP 19
.ti +4
INTEGER
IFAIL( * ), IWORK( * )
.TP 19
.ti +4
REAL
RWORK( * ), W( * )
.TP 19
.ti +4
COMPLEX
AB( LDAB, * ), Q( LDQ, * ), WORK( * ),
Z( LDZ, * )
.SH PURPOSE
CHBEVX computes selected eigenvalues and, optionally, eigenvectors
of a complex Hermitian band matrix A.  Eigenvalues and eigenvectors
can be selected by specifying either a range of values or a range of
indices for the desired eigenvalues.
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
= \(aqA\(aq: all eigenvalues will be found;
.br
= \(aqV\(aq: all eigenvalues in the half-open interval (VL,VU]
will be found;
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
KD      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input/output) COMPLEX array, dimension (LDAB, N)
On entry, the upper or lower triangle of the Hermitian band
matrix A, stored in the first KD+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).

On exit, AB is overwritten by values generated during the
reduction to tridiagonal form.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD + 1.
.TP 8
Q       (output) COMPLEX array, dimension (LDQ, N)
If JOBZ = \(aqV\(aq, the N-by-N unitary matrix used in the
reduction to tridiagonal form.
If JOBZ = \(aqN\(aq, the array Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  If JOBZ = \(aqV\(aq, then
LDQ >= max(1,N).
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
1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
Not referenced if RANGE = \(aqA\(aq or \(aqV\(aq.
.TP 8
ABSTOL  (input) REAL
The absolute error tolerance for the eigenvalues.
An approximate eigenvalue is accepted as converged
when it is determined to lie in an interval [a,b]
of width less than or equal to

ABSTOL + EPS *   max( |a|,|b| ) ,

where EPS is the machine precision.  If ABSTOL is less than
or equal to zero, then  EPS*|T|  will be used in its place,
where |T| is the 1-norm of the tridiagonal matrix obtained
by reducing AB to tridiagonal form.

Eigenvalues will be computed most accurately when ABSTOL is
set to twice the underflow threshold 2*SLAMCH(\(aqS\(aq), not zero.
If this routine returns with INFO>0, indicating that some
eigenvectors did not converge, try setting ABSTOL to
2*SLAMCH(\(aqS\(aq).

See "Computing Small Singular Values of Bidiagonal Matrices
with Guaranteed High Relative Accuracy," by Demmel and
Kahan, LAPACK Working Note #3.
.TP 8
M       (output) INTEGER
The total number of eigenvalues found.  0 <= M <= N.
If RANGE = \(aqA\(aq, M = N, and if RANGE = \(aqI\(aq, M = IU-IL+1.
.TP 8
W       (output) REAL array, dimension (N)
The first M elements contain the selected eigenvalues in
ascending order.
.TP 8
Z       (output) COMPLEX array, dimension (LDZ, max(1,M))
If JOBZ = \(aqV\(aq, then if INFO = 0, the first M columns of Z
contain the orthonormal eigenvectors of the matrix A
corresponding to the selected eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
If an eigenvector fails to converge, then that column of Z
contains the latest approximation to the eigenvector, and the
index of the eigenvector is returned in IFAIL.
If JOBZ = \(aqN\(aq, then Z is not referenced.
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z; if RANGE = \(aqV\(aq, the exact value of M
is not known in advance and an upper bound must be used.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace) COMPLEX array, dimension (N)
.TP 8
RWORK   (workspace) REAL array, dimension (7*N)
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
