.TH  SSBGV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
SSBGV - all the eigenvalues, and optionally, the eigenvectors of a real generalized symmetric-definite banded eigenproblem, of the form A*x=(lambda)*B*x
.SH SYNOPSIS
.TP 18
SUBROUTINE SSBGV(
JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W, Z,
LDZ, WORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBZ, UPLO
.TP 18
.ti +4
INTEGER
INFO, KA, KB, LDAB, LDBB, LDZ, N
.TP 18
.ti +4
REAL
AB( LDAB, * ), BB( LDBB, * ), W( * ),
WORK( * ), Z( LDZ, * )
.SH PURPOSE
SSBGV computes all the eigenvalues, and optionally, the eigenvectors
of a real generalized symmetric-definite banded eigenproblem, of
the form A*x=(lambda)*B*x. Here A and B are assumed to be symmetric
and banded, and B is also positive definite.
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
AB      (input/output) REAL array, dimension (LDAB, N)
On entry, the upper or lower triangle of the symmetric band
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
BB      (input/output) REAL array, dimension (LDBB, N)
On entry, the upper or lower triangle of the symmetric band
matrix B, stored in the first kb+1 rows of the array.  The
j-th column of B is stored in the j-th column of the array BB
as follows:
if UPLO = \(aqU\(aq, BB(kb+1+i-j,j) = B(i,j) for max(1,j-kb)<=i<=j;
if UPLO = \(aqL\(aq, BB(1+i-j,j)    = B(i,j) for j<=i<=min(n,j+kb).

On exit, the factor S from the split Cholesky factorization
B = S**T*S, as returned by SPBSTF.
.TP 8
LDBB    (input) INTEGER
The leading dimension of the array BB.  LDBB >= KB+1.
.TP 8
W       (output) REAL array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
Z       (output) REAL array, dimension (LDZ, N)
If JOBZ = \(aqV\(aq, then if INFO = 0, Z contains the matrix Z of
eigenvectors, with the i-th column of Z holding the
eigenvector associated with W(i). The eigenvectors are
normalized so that Z**T*B*Z = I.
If JOBZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= N.
.TP 8
WORK    (workspace) REAL array, dimension (3*N)
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
> N:   if INFO = N + i, for 1 <= i <= N, then SPBSTF
.br
returned INFO = i: B is not positive definite.
The factorization of B could not be completed and
no eigenvalues or eigenvectors were computed.
