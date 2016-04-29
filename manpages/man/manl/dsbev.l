.TH  DSBEV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DSBEV - all the eigenvalues and, optionally, eigenvectors of a real symmetric band matrix A
.SH SYNOPSIS
.TP 18
SUBROUTINE DSBEV(
JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,
INFO )
.TP 18
.ti +4
CHARACTER
JOBZ, UPLO
.TP 18
.ti +4
INTEGER
INFO, KD, LDAB, LDZ, N
.TP 18
.ti +4
DOUBLE
PRECISION AB( LDAB, * ), W( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
DSBEV computes all the eigenvalues and, optionally, eigenvectors of
a real symmetric band matrix A.

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
KD      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
On entry, the upper or lower triangle of the symmetric band
matrix A, stored in the first KD+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).

On exit, AB is overwritten by values generated during the
reduction to tridiagonal form.  If UPLO = \(aqU\(aq, the first
superdiagonal and the diagonal of the tridiagonal matrix T
are returned in rows KD and KD+1 of AB, and if UPLO = \(aqL\(aq,
the diagonal and first subdiagonal of T are returned in the
first two rows of AB.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD + 1.
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
If JOBZ = \(aqV\(aq, then if INFO = 0, Z contains the orthonormal
eigenvectors of the matrix A, with the i-th column of Z
holding the eigenvector associated with W(i).
If JOBZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,3*N-2))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of an intermediate tridiagonal
form did not converge to zero.
