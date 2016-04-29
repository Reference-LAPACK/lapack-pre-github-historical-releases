.TH  DSPGVD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DSPGVD - all the eigenvalues, and optionally, the eigenvectors of a real generalized symmetric-definite eigenproblem, of the form A*x=(lambda)*B*x, A*Bx=(lambda)*x, or B*A*x=(lambda)*x
.SH SYNOPSIS
.TP 19
SUBROUTINE DSPGVD(
ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,
LWORK, IWORK, LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, UPLO
.TP 19
.ti +4
INTEGER
INFO, ITYPE, LDZ, LIWORK, LWORK, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION AP( * ), BP( * ), W( * ), WORK( * ),
Z( LDZ, * )
.SH PURPOSE
DSPGVD computes all the eigenvalues, and optionally, the eigenvectors
of a real generalized symmetric-definite eigenproblem, of the form
A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
B are assumed to be symmetric, stored in packed format, and B is also
positive definite.
.br
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
AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the symmetric matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.

On exit, the contents of AP are destroyed.
.TP 8
BP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the symmetric matrix
B, packed columnwise in a linear array.  The j-th column of B
is stored in the array BP as follows:
if UPLO = \(aqU\(aq, BP(i + (j-1)*j/2) = B(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, BP(i + (j-1)*(2*n-j)/2) = B(i,j) for j<=i<=n.

On exit, the triangular factor U or L from the Cholesky
factorization B = U**T*U or B = L*L**T, in the same storage
format as B.
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
If JOBZ = \(aqV\(aq, then if INFO = 0, Z contains the matrix Z of
eigenvectors.  The eigenvectors are normalized as follows:
if ITYPE = 1 or 2, Z**T*B*Z = I;
if ITYPE = 3, Z**T*inv(B)*Z = I.
If JOBZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the required LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If N <= 1,               LWORK >= 1.
If JOBZ = \(aqN\(aq and N > 1, LWORK >= 2*N.
If JOBZ = \(aqV\(aq and N > 1, LWORK >= 1 + 6*N + 2*N**2.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the required sizes of the WORK and IWORK
arrays, returns these values as the first entries of the WORK
and IWORK arrays, and no error message related to LWORK or
LIWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
On exit, if INFO = 0, IWORK(1) returns the required LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK.
If JOBZ  = \(aqN\(aq or N <= 1, LIWORK >= 1.
If JOBZ  = \(aqV\(aq and N > 1, LIWORK >= 3 + 5*N.

If LIWORK = -1, then a workspace query is assumed; the
routine only calculates the required sizes of the WORK and
IWORK arrays, returns these values as the first entries of
the WORK and IWORK arrays, and no error message related to
LWORK or LIWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  DPPTRF or DSPEVD returned an error code:
.br
<= N:  if INFO = i, DSPEVD failed to converge;
i off-diagonal elements of an intermediate
tridiagonal form did not converge to zero;
> N:   if INFO = N + i, for 1 <= i <= N, then the leading
minor of order i of B is not positive definite.
The factorization of B could not be completed and
no eigenvalues or eigenvectors were computed.
.SH FURTHER DETAILS
Based on contributions by
.br
   Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA

