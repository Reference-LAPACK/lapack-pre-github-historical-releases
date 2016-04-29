.TH  SSTEQR 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SSTEQR - all eigenvalues and, optionally, eigenvectors of a symmetric tridiagonal matrix using the implicit QL or QR method
.SH SYNOPSIS
.TP 19
SUBROUTINE SSTEQR(
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
REAL
D( * ), E( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
SSTEQR computes all eigenvalues and, optionally, eigenvectors of a
symmetric tridiagonal matrix using the implicit QL or QR method.
The eigenvectors of a full or band symmetric matrix can also be found
if SSYTRD or SSPTRD or SSBTRD has been used to reduce this matrix to
tridiagonal form.
.br

.SH ARGUMENTS
.TP 8
COMPZ   (input) CHARACTER*1
= \(aqN\(aq:  Compute eigenvalues only.
.br
= \(aqV\(aq:  Compute eigenvalues and eigenvectors of the original
symmetric matrix.  On entry, Z must contain the
orthogonal matrix used to reduce the original matrix
to tridiagonal form.
= \(aqI\(aq:  Compute eigenvalues and eigenvectors of the
tridiagonal matrix.  Z is initialized to the identity
matrix.
.TP 8
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the diagonal elements of the tridiagonal matrix.
On exit, if INFO = 0, the eigenvalues in ascending order.
.TP 8
E       (input/output) REAL array, dimension (N-1)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix.
On exit, E has been destroyed.
.TP 8
Z       (input/output) REAL array, dimension (LDZ, N)
On entry, if  COMPZ = \(aqV\(aq, then Z contains the orthogonal
matrix used in the reduction to tridiagonal form.
On exit, if INFO = 0, then if  COMPZ = \(aqV\(aq, Z contains the
orthonormal eigenvectors of the original symmetric matrix,
and if COMPZ = \(aqI\(aq, Z contains the orthonormal eigenvectors
of the symmetric tridiagonal matrix.
If COMPZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
eigenvectors are desired, then  LDZ >= max(1,N).
.TP 8
WORK    (workspace) REAL array, dimension (max(1,2*N-2))
If COMPZ = \(aqN\(aq, then WORK is not referenced.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  the algorithm has failed to find all the eigenvalues in
a total of 30*N iterations; if INFO = i, then i
elements of E have not converged to zero; on exit, D
and E contain the elements of a symmetric tridiagonal
matrix which is orthogonally similar to the original
matrix.
