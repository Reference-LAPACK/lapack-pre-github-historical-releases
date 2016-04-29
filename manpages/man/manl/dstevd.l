.TH  DSTEVD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DSTEVD - all eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE DSTEVD(
JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,
LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ
.TP 19
.ti +4
INTEGER
INFO, LDZ, LIWORK, LWORK, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
DSTEVD computes all eigenvalues and, optionally, eigenvectors of a
real symmetric tridiagonal matrix. If eigenvectors are desired, it
uses a divide and conquer algorithm.
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
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the n diagonal elements of the tridiagonal matrix
A.
On exit, if INFO = 0, the eigenvalues in ascending order.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N-1)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix A, stored in elements 1 to N-1 of E.
On exit, the contents of E are destroyed.
.TP 8
Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
If JOBZ = \(aqV\(aq, then if INFO = 0, Z contains the orthonormal
eigenvectors of the matrix A, with the i-th column of Z
holding the eigenvector associated with D(i).
If JOBZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = \(aqV\(aq, LDZ >= max(1,N).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array,
dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If JOBZ  = \(aqN\(aq or N <= 1 then LWORK must be at least 1.
If JOBZ  = \(aqV\(aq and N > 1 then LWORK must be at least
( 1 + 4*N + N**2 ).

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
If JOBZ  = \(aqN\(aq or N <= 1 then LIWORK must be at least 1.
If JOBZ  = \(aqV\(aq and N > 1 then LIWORK must be at least 3+5*N.

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
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of E did not converge to zero.
