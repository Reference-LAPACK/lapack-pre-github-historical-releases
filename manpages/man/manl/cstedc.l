.TH  CSTEDC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CSTEDC - all eigenvalues and, optionally, eigenvectors of a symmetric tridiagonal matrix using the divide and conquer method
.SH SYNOPSIS
.TP 19
SUBROUTINE CSTEDC(
COMPZ, N, D, E, Z, LDZ, WORK, LWORK, RWORK,
LRWORK, IWORK, LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
COMPZ
.TP 19
.ti +4
INTEGER
INFO, LDZ, LIWORK, LRWORK, LWORK, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
D( * ), E( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX
WORK( * ), Z( LDZ, * )
.SH PURPOSE
CSTEDC computes all eigenvalues and, optionally, eigenvectors of a
symmetric tridiagonal matrix using the divide and conquer method.
The eigenvectors of a full or band complex Hermitian matrix can also
be found if CHETRD or CHPTRD or CHBTRD has been used to reduce this
matrix to tridiagonal form.
.br

This code makes very mild assumptions about floating point
arithmetic. It will work on machines with a guard digit in
add/subtract, or on those binary machines without guard digits
which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.  See SLAED3 for details.

.SH ARGUMENTS
.TP 8
COMPZ   (input) CHARACTER*1
= \(aqN\(aq:  Compute eigenvalues only.
.br
= \(aqI\(aq:  Compute eigenvectors of tridiagonal matrix also.
.br
= \(aqV\(aq:  Compute eigenvectors of original Hermitian matrix
also.  On entry, Z contains the unitary matrix used
to reduce the original matrix to tridiagonal form.
.TP 8
N       (input) INTEGER
The dimension of the symmetric tridiagonal matrix.  N >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the diagonal elements of the tridiagonal matrix.
On exit, if INFO = 0, the eigenvalues in ascending order.
.TP 8
E       (input/output) REAL array, dimension (N-1)
On entry, the subdiagonal elements of the tridiagonal matrix.
On exit, E has been destroyed.
.TP 8
Z       (input/output) COMPLEX array, dimension (LDZ,N)
On entry, if COMPZ = \(aqV\(aq, then Z contains the unitary
matrix used in the reduction to tridiagonal form.
On exit, if INFO = 0, then if COMPZ = \(aqV\(aq, Z contains the
orthonormal eigenvectors of the original Hermitian matrix,
and if COMPZ = \(aqI\(aq, Z contains the orthonormal eigenvectors
of the symmetric tridiagonal matrix.
If  COMPZ = \(aqN\(aq, then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1.
If eigenvectors are desired, then LDZ >= max(1,N).
.TP 8
WORK    (workspace/output) COMPLEX    array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If COMPZ = \(aqN\(aq or \(aqI\(aq, or N <= 1, LWORK must be at least 1.
If COMPZ = \(aqV\(aq and N > 1, LWORK must be at least N*N.
Note that for COMPZ = \(aqV\(aq, then if N is less than or
equal to the minimum divide size, usually 25, then LWORK need
only be 1.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal sizes of the WORK, RWORK and
IWORK arrays, returns these values as the first entries of
the WORK, RWORK and IWORK arrays, and no error message
related to LWORK or LRWORK or LIWORK is issued by XERBLA.
.TP 8
RWORK   (workspace/output) REAL array, dimension (MAX(1,LRWORK))
On exit, if INFO = 0, RWORK(1) returns the optimal LRWORK.
.TP 8
LRWORK  (input) INTEGER
The dimension of the array RWORK.
If COMPZ = \(aqN\(aq or N <= 1, LRWORK must be at least 1.
If COMPZ = \(aqV\(aq and N > 1, LRWORK must be at least
1 + 3*N + 2*N*lg N + 3*N**2 ,
where lg( N ) = smallest integer k such
that 2**k >= N.
If COMPZ = \(aqI\(aq and N > 1, LRWORK must be at least
1 + 4*N + 2*N**2 .
Note that for COMPZ = \(aqI\(aq or \(aqV\(aq, then if N is less than or
equal to the minimum divide size, usually 25, then LRWORK
need only be max(1,2*(N-1)).

If LRWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal sizes of the WORK, RWORK
and IWORK arrays, returns these values as the first entries
of the WORK, RWORK and IWORK arrays, and no error message
related to LWORK or LRWORK or LIWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK.
If COMPZ = \(aqN\(aq or N <= 1, LIWORK must be at least 1.
If COMPZ = \(aqV\(aq or N > 1,  LIWORK must be at least
6 + 6*N + 5*N*lg N.
If COMPZ = \(aqI\(aq or N > 1,  LIWORK must be at least
3 + 5*N .
Note that for COMPZ = \(aqI\(aq or \(aqV\(aq, then if N is less than or
equal to the minimum divide size, usually 25, then LIWORK
need only be 1.

If LIWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal sizes of the WORK, RWORK
and IWORK arrays, returns these values as the first entries
of the WORK, RWORK and IWORK arrays, and no error message
related to LWORK or LRWORK or LIWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  The algorithm failed to compute an eigenvalue while
working on the submatrix lying in rows and columns
INFO/(N+1) through mod(INFO,N+1).
.SH FURTHER DETAILS
Based on contributions by
.br
   Jeff Rutter, Computer Science Division, University of California
   at Berkeley, USA
.br

