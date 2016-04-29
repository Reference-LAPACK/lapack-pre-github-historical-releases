.TH  SSTEV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
SSTEV - all eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix A
.SH SYNOPSIS
.TP 18
SUBROUTINE SSTEV(
JOBZ, N, D, E, Z, LDZ, WORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBZ
.TP 18
.ti +4
INTEGER
INFO, LDZ, N
.TP 18
.ti +4
REAL
D( * ), E( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
SSTEV computes all eigenvalues and, optionally, eigenvectors of a
real symmetric tridiagonal matrix A.

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
= \(aqN\(aq:  Compute eigenvalues only;
.br
= \(aqV\(aq:  Compute eigenvalues and eigenvectors.
.TP 8
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the n diagonal elements of the tridiagonal matrix
A.
On exit, if INFO = 0, the eigenvalues in ascending order.
.TP 8
E       (input/output) REAL array, dimension (N-1)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix A, stored in elements 1 to N-1 of E.
On exit, the contents of E are destroyed.
.TP 8
Z       (output) REAL array, dimension (LDZ, N)
If JOBZ = \(aqV\(aq, then if INFO = 0, Z contains the orthonormal
eigenvectors of the matrix A, with the i-th column of Z
holding the eigenvector associated with D(i).
If JOBZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace) REAL array, dimension (max(1,2*N-2))
If JOBZ = \(aqN\(aq, WORK is not referenced.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of E did not converge to zero.
