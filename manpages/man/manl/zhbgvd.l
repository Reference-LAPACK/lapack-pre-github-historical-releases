.TH  ZHBGVD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZHBGVD - all the eigenvalues, and optionally, the eigenvectors of a complex generalized Hermitian-definite banded eigenproblem, of the form A*x=(lambda)*B*x
.SH SYNOPSIS
.TP 19
SUBROUTINE ZHBGVD(
JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,
Z, LDZ, WORK, LWORK, RWORK, LRWORK, IWORK,
LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, UPLO
.TP 19
.ti +4
INTEGER
INFO, KA, KB, LDAB, LDBB, LDZ, LIWORK, LRWORK,
LWORK, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * ), W( * )
.TP 19
.ti +4
COMPLEX*16
AB( LDAB, * ), BB( LDBB, * ), WORK( * ),
Z( LDZ, * )
.SH PURPOSE
ZHBGVD computes all the eigenvalues, and optionally, the eigenvectors
of a complex generalized Hermitian-definite banded eigenproblem, of
the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
and banded, and B is also positive definite.  If eigenvectors are
desired, it uses a divide and conquer algorithm.
.br

The divide and conquer algorithm makes very mild assumptions about
floating point arithmetic. It will work on machines with a guard
digit in add/subtract, or on those binary machines without guard
digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
Cray-2. It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
.br

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
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
KA      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq. KA >= 0.
.TP 8
KB      (input) INTEGER
The number of superdiagonals of the matrix B if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq. KB >= 0.
.TP 8
AB      (input/output) COMPLEX*16 array, dimension (LDAB, N)
On entry, the upper or lower triangle of the Hermitian band
matrix A, stored in the first ka+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = \(aqU\(aq, AB(ka+1+i-j,j) = A(i,j) for max(1,j-ka)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+ka).

On exit, the contents of AB are destroyed.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KA+1.
.TP 8
BB      (input/output) COMPLEX*16 array, dimension (LDBB, N)
On entry, the upper or lower triangle of the Hermitian band
matrix B, stored in the first kb+1 rows of the array.  The
j-th column of B is stored in the j-th column of the array BB
as follows:
if UPLO = \(aqU\(aq, BB(kb+1+i-j,j) = B(i,j) for max(1,j-kb)<=i<=j;
if UPLO = \(aqL\(aq, BB(1+i-j,j)    = B(i,j) for j<=i<=min(n,j+kb).

On exit, the factor S from the split Cholesky factorization
B = S**H*S, as returned by ZPBSTF.
.TP 8
LDBB    (input) INTEGER
The leading dimension of the array BB.  LDBB >= KB+1.
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
Z       (output) COMPLEX*16 array, dimension (LDZ, N)
If JOBZ = \(aqV\(aq, then if INFO = 0, Z contains the matrix Z of
eigenvectors, with the i-th column of Z holding the
eigenvector associated with W(i). The eigenvectors are
normalized so that Z**H*B*Z = I.
If JOBZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= N.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO=0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If N <= 1,               LWORK >= 1.
If JOBZ = \(aqN\(aq and N > 1, LWORK >= N.
If JOBZ = \(aqV\(aq and N > 1, LWORK >= 2*N**2.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal sizes of the WORK, RWORK and
IWORK arrays, returns these values as the first entries of
the WORK, RWORK and IWORK arrays, and no error message
related to LWORK or LRWORK or LIWORK is issued by XERBLA.
.TP 8
RWORK   (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LRWORK))
On exit, if INFO=0, RWORK(1) returns the optimal LRWORK.
.TP 8
LRWORK  (input) INTEGER
The dimension of array RWORK.
If N <= 1,               LRWORK >= 1.
If JOBZ = \(aqN\(aq and N > 1, LRWORK >= N.
If JOBZ = \(aqV\(aq and N > 1, LRWORK >= 1 + 5*N + 2*N**2.

If LRWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal sizes of the WORK, RWORK
and IWORK arrays, returns these values as the first entries
of the WORK, RWORK and IWORK arrays, and no error message
related to LWORK or LRWORK or LIWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
On exit, if INFO=0, IWORK(1) returns the optimal LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of array IWORK.
If JOBZ = \(aqN\(aq or N <= 1, LIWORK >= 1.
If JOBZ = \(aqV\(aq and N > 1, LIWORK >= 3 + 5*N.

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
> 0:  if INFO = i, and i is:
.br
<= N:  the algorithm failed to converge:
i off-diagonal elements of an intermediate
tridiagonal form did not converge to zero;
> N:   if INFO = N + i, for 1 <= i <= N, then ZPBSTF
.br
returned INFO = i: B is not positive definite.
The factorization of B could not be completed and
no eigenvalues or eigenvectors were computed.
.SH FURTHER DETAILS
Based on contributions by
.br
   Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA

