.TH  CHPEV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CHPEV - all the eigenvalues and, optionally, eigenvectors of a complex Hermitian matrix in packed storage
.SH SYNOPSIS
.TP 18
SUBROUTINE CHPEV(
JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, RWORK,
INFO )
.TP 18
.ti +4
CHARACTER
JOBZ, UPLO
.TP 18
.ti +4
INTEGER
INFO, LDZ, N
.TP 18
.ti +4
REAL
RWORK( * ), W( * )
.TP 18
.ti +4
COMPLEX
AP( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
CHPEV computes all the eigenvalues and, optionally, eigenvectors of a
complex Hermitian matrix in packed storage.

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
= \(aqN\(aq:  Compute eigenvalues only;
.br
= \(aqV\(aq:  Compute eigenvalues and eigenvectors.
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
AP      (input/output) COMPLEX array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the Hermitian matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.

On exit, AP is overwritten by values generated during the
reduction to tridiagonal form.  If UPLO = \(aqU\(aq, the diagonal
and first superdiagonal of the tridiagonal matrix T overwrite
the corresponding elements of A, and if UPLO = \(aqL\(aq, the
diagonal and first subdiagonal of T overwrite the
corresponding elements of A.
.TP 8
W       (output) REAL array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
Z       (output) COMPLEX array, dimension (LDZ, N)
If JOBZ = \(aqV\(aq, then if INFO = 0, Z contains the orthonormal
eigenvectors of the matrix A, with the i-th column of Z
holding the eigenvector associated with W(i).
If JOBZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace) COMPLEX array, dimension (max(1, 2*N-1))
.TP 8
RWORK   (workspace) REAL array, dimension (max(1, 3*N-2))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of an intermediate tridiagonal
form did not converge to zero.
