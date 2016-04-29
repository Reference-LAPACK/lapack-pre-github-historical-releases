.TH  CHEGV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CHEGV - all the eigenvalues, and optionally, the eigenvectors of a complex generalized Hermitian-definite eigenproblem, of the form A*x=(lambda)*B*x, A*Bx=(lambda)*x, or B*A*x=(lambda)*x
.SH SYNOPSIS
.TP 18
SUBROUTINE CHEGV(
ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,
LWORK, RWORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBZ, UPLO
.TP 18
.ti +4
INTEGER
INFO, ITYPE, LDA, LDB, LWORK, N
.TP 18
.ti +4
REAL
RWORK( * ), W( * )
.TP 18
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), WORK( * )
.SH PURPOSE
CHEGV computes all the eigenvalues, and optionally, the eigenvectors
of a complex generalized Hermitian-definite eigenproblem, of the form
A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
Here A and B are assumed to be Hermitian and B is also
.br
positive definite.
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
A       (input/output) COMPLEX array, dimension (LDA, N)
On entry, the Hermitian matrix A.  If UPLO = \(aqU\(aq, the
leading N-by-N upper triangular part of A contains the
upper triangular part of the matrix A.  If UPLO = \(aqL\(aq,
the leading N-by-N lower triangular part of A contains
the lower triangular part of the matrix A.

On exit, if JOBZ = \(aqV\(aq, then if INFO = 0, A contains the
matrix Z of eigenvectors.  The eigenvectors are normalized
as follows:
if ITYPE = 1 or 2, Z**H*B*Z = I;
if ITYPE = 3, Z**H*inv(B)*Z = I.
If JOBZ = \(aqN\(aq, then on exit the upper triangle (if UPLO=\(aqU\(aq)
or the lower triangle (if UPLO=\(aqL\(aq) of A, including the
diagonal, is destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB, N)
On entry, the Hermitian positive definite matrix B.
If UPLO = \(aqU\(aq, the leading N-by-N upper triangular part of B
contains the upper triangular part of the matrix B.
If UPLO = \(aqL\(aq, the leading N-by-N lower triangular part of B
contains the lower triangular part of the matrix B.

On exit, if INFO <= N, the part of B containing the matrix is
overwritten by the triangular factor U or L from the Cholesky
factorization B = U**H*U or B = L*L**H.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
W       (output) REAL array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of the array WORK.  LWORK >= max(1,2*N-1).
For optimal efficiency, LWORK >= (NB+1)*N,
where NB is the blocksize for CHETRD returned by ILAENV.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
RWORK   (workspace) REAL array, dimension (max(1, 3*N-2))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  CPOTRF or CHEEV returned an error code:
.br
<= N:  if INFO = i, CHEEV failed to converge;
i off-diagonal elements of an intermediate
tridiagonal form did not converge to zero;
> N:   if INFO = N + i, for 1 <= i <= N, then the leading
minor of order i of B is not positive definite.
The factorization of B could not be completed and
no eigenvalues or eigenvectors were computed.
