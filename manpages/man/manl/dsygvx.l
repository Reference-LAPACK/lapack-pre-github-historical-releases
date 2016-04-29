.TH  DSYGVX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DSYGVX - selected eigenvalues, and optionally, eigenvectors of a real generalized symmetric-definite eigenproblem, of the form A*x=(lambda)*B*x, A*Bx=(lambda)*x, or B*A*x=(lambda)*x
.SH SYNOPSIS
.TP 19
SUBROUTINE DSYGVX(
ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,
VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,
LWORK, IWORK, IFAIL, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, RANGE, UPLO
.TP 19
.ti +4
INTEGER
IL, INFO, ITYPE, IU, LDA, LDB, LDZ, LWORK, M, N
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
PRECISION A( LDA, * ), B( LDB, * ), W( * ), WORK( * ),
Z( LDZ, * )
.SH PURPOSE
DSYGVX computes selected eigenvalues, and optionally, eigenvectors
of a real generalized symmetric-definite eigenproblem, of the form
A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
and B are assumed to be symmetric and B is also positive definite.
Eigenvalues and eigenvectors can be selected by specifying either a
range of values or a range of indices for the desired eigenvalues.

.SH ARGUMENTS
.TP 8
ITYPE   (input) INTEGER
Specifies the problem type to be solved:
.br
= 1:  A*x = (lambda)*B*x
.br
= 2:  A*B*x = (lambda)*x
.br
= 3:  B*A*x = (lambda)*x
.TP 8
JOBZ    (input) CHARACTER*1
.br
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
= \(aqU\(aq:  Upper triangle of A and B are stored;
.br
= \(aqL\(aq:  Lower triangle of A and B are stored.
.TP 8
N       (input) INTEGER
The order of the matrix pencil (A,B).  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the
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
B       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
On entry, the symmetric matrix B.  If UPLO = \(aqU\(aq, the
leading N-by-N upper triangular part of B contains the
upper triangular part of the matrix B.  If UPLO = \(aqL\(aq,
the leading N-by-N lower triangular part of B contains
the lower triangular part of the matrix B.

On exit, if INFO <= N, the part of B containing the matrix is
overwritten by the triangular factor U or L from the Cholesky
factorization B = U**T*U or B = L*L**T.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
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

Eigenvalues will be computed most accurately when ABSTOL is
set to twice the underflow threshold 2*DLAMCH(\(aqS\(aq), not zero.
If this routine returns with INFO>0, indicating that some
eigenvectors did not converge, try setting ABSTOL to
2*DLAMCH(\(aqS\(aq).
.TP 8
M       (output) INTEGER
The total number of eigenvalues found.  0 <= M <= N.
If RANGE = \(aqA\(aq, M = N, and if RANGE = \(aqI\(aq, M = IU-IL+1.
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
On normal exit, the first M elements contain the selected
eigenvalues in ascending order.
.TP 8
Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
If JOBZ = \(aqN\(aq, then Z is not referenced.
If JOBZ = \(aqV\(aq, then if INFO = 0, the first M columns of Z
contain the orthonormal eigenvectors of the matrix A
corresponding to the selected eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
The eigenvectors are normalized as follows:
if ITYPE = 1 or 2, Z**T*B*Z = I;
if ITYPE = 3, Z**T*inv(B)*Z = I.

If an eigenvector fails to converge, then that column of Z
contains the latest approximation to the eigenvector, and the
index of the eigenvector is returned in IFAIL.
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z; if RANGE = \(aqV\(aq, the exact value of M
is not known in advance and an upper bound must be used.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of the array WORK.  LWORK >= max(1,8*N).
For optimal efficiency, LWORK >= (NB+3)*N,
where NB is the blocksize for DSYTRD returned by ILAENV.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
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
> 0:  DPOTRF or DSYEVX returned an error code:
.br
<= N:  if INFO = i, DSYEVX failed to converge;
i eigenvectors failed to converge.  Their indices
are stored in array IFAIL.
> N:   if INFO = N + i, for 1 <= i <= N, then the leading
minor of order i of B is not positive definite.
The factorization of B could not be completed and
no eigenvalues or eigenvectors were computed.
.SH FURTHER DETAILS
Based on contributions by
.br
   Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA

