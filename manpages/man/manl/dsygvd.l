.TH  DSYGVD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DSYGVD - all the eigenvalues, and optionally, the eigenvectors of a real generalized symmetric-definite eigenproblem, of the form A*x=(lambda)*B*x, A*Bx=(lambda)*x, or B*A*x=(lambda)*x
.SH SYNOPSIS
.TP 19
SUBROUTINE DSYGVD(
ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,
LWORK, IWORK, LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, UPLO
.TP 19
.ti +4
INTEGER
INFO, ITYPE, LDA, LDB, LIWORK, LWORK, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), W( * ), WORK( * )
.SH PURPOSE
DSYGVD computes all the eigenvalues, and optionally, the eigenvectors
of a real generalized symmetric-definite eigenproblem, of the form
A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
B are assumed to be symmetric and B is also positive definite.
If eigenvectors are desired, it uses a divide and conquer algorithm.

The divide and conquer algorithm makes very mild assumptions about
floating point arithmetic. It will work on machines with a guard
digit in add/subtract, or on those binary machines without guard
digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
Cray-2. It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
.br

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
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq:  Upper triangles of A and B are stored;
.br
= \(aqL\(aq:  Lower triangles of A and B are stored.
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the
leading N-by-N upper triangular part of A contains the
upper triangular part of the matrix A.  If UPLO = \(aqL\(aq,
the leading N-by-N lower triangular part of A contains
the lower triangular part of the matrix A.

On exit, if JOBZ = \(aqV\(aq, then if INFO = 0, A contains the
matrix Z of eigenvectors.  The eigenvectors are normalized
as follows:
if ITYPE = 1 or 2, Z**T*B*Z = I;
if ITYPE = 3, Z**T*inv(B)*Z = I.
If JOBZ = \(aqN\(aq, then on exit the upper triangle (if UPLO=\(aqU\(aq)
or the lower triangle (if UPLO=\(aqL\(aq) of A, including the
diagonal, is destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
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
W       (output) DOUBLE PRECISION array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If N <= 1,               LWORK >= 1.
If JOBZ = \(aqN\(aq and N > 1, LWORK >= 2*N+1.
If JOBZ = \(aqV\(aq and N > 1, LWORK >= 1 + 6*N + 2*N**2.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal sizes of the WORK and IWORK
arrays, returns these values as the first entries of the WORK
and IWORK arrays, and no error message related to LWORK or
LIWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK.
If N <= 1,                LIWORK >= 1.
If JOBZ  = \(aqN\(aq and N > 1, LIWORK >= 1.
If JOBZ  = \(aqV\(aq and N > 1, LIWORK >= 3 + 5*N.

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
> 0:  DPOTRF or DSYEVD returned an error code:
.br
<= N:  if INFO = i and JOBZ = \(aqN\(aq, then the algorithm
failed to converge; i off-diagonal elements of an
intermediate tridiagonal form did not converge to
zero;
if INFO = i and JOBZ = \(aqV\(aq, then the algorithm
failed to compute an eigenvalue while working on
the submatrix lying in rows and columns INFO/(N+1)
through mod(INFO,N+1);
> N:   if INFO = N + i, for 1 <= i <= N, then the leading
minor of order i of B is not positive definite.
The factorization of B could not be completed and
no eigenvalues or eigenvectors were computed.
.SH FURTHER DETAILS
Based on contributions by
.br
   Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA

Modified so that no backsubstitution is performed if DSYEVD fails to
converge (NEIG in old code could be greater than N causing out of
bounds reference to A - reported by Ralf Meyer).  Also corrected the
description of INFO and the test on ITYPE. Sven, 16 Feb 05.
