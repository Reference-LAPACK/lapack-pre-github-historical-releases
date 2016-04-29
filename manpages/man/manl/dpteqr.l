.TH  DPTEQR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DPTEQR - all eigenvalues and, optionally, eigenvectors of a symmetric positive definite tridiagonal matrix by first factoring the matrix using DPTTRF, and then calling DBDSQR to compute the singular values of the bidiagonal factor
.SH SYNOPSIS
.TP 19
SUBROUTINE DPTEQR(
COMPZ, N, D, E, Z, LDZ, WORK, INFO )
.TP 19
.ti +4
CHARACTER
COMPZ
.TP 19
.ti +4
INTEGER
INFO, LDZ, N
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
DPTEQR computes all eigenvalues and, optionally, eigenvectors of a
symmetric positive definite tridiagonal matrix by first factoring the
matrix using DPTTRF, and then calling DBDSQR to compute the singular
values of the bidiagonal factor.

This routine computes the eigenvalues of the positive definite
tridiagonal matrix to high relative accuracy.  This means that if the
eigenvalues range over many orders of magnitude in size, then the
small eigenvalues and corresponding eigenvectors will be computed
more accurately than, for example, with the standard QR method.

The eigenvectors of a full or band symmetric positive definite matrix
can also be found if DSYTRD, DSPTRD, or DSBTRD has been used to
reduce this matrix to tridiagonal form. (The reduction to tridiagonal
form, however, may preclude the possibility of obtaining high
relative accuracy in the small eigenvalues of the original matrix, if
these eigenvalues range over many orders of magnitude.)
.br

.SH ARGUMENTS
.TP 8
COMPZ   (input) CHARACTER*1
= \(aqN\(aq:  Compute eigenvalues only.
.br
= \(aqV\(aq:  Compute eigenvectors of original symmetric
matrix also.  Array Z contains the orthogonal
matrix used to reduce the original matrix to
tridiagonal form.
= \(aqI\(aq:  Compute eigenvectors of tridiagonal matrix also.
.TP 8
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the n diagonal elements of the tridiagonal
matrix.
On normal exit, D contains the eigenvalues, in descending
order.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N-1)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix.
On exit, E has been destroyed.
.TP 8
Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
On entry, if COMPZ = \(aqV\(aq, the orthogonal matrix used in the
reduction to tridiagonal form.
On exit, if COMPZ = \(aqV\(aq, the orthonormal eigenvectors of the
original symmetric matrix;
if COMPZ = \(aqI\(aq, the orthonormal eigenvectors of the
tridiagonal matrix.
If INFO > 0 on exit, Z contains the eigenvectors associated
with only the stored eigenvalues.
If  COMPZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
COMPZ = \(aqV\(aq or \(aqI\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = i, and i is:
<= N  the Cholesky factorization of the matrix could
not be performed because the i-th principal minor
was not positive definite.
> N   the SVD algorithm failed to converge;
if INFO = N+i, i off-diagonal elements of the
bidiagonal factor did not converge to zero.
