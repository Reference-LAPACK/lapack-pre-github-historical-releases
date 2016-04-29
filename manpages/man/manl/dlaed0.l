.TH  DLAED0 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DLAED0 - all eigenvalues and corresponding eigenvectors of a symmetric tridiagonal matrix using the divide and conquer method
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAED0(
ICOMPQ, QSIZ, N, D, E, Q, LDQ, QSTORE, LDQS,
WORK, IWORK, INFO )
.TP 19
.ti +4
INTEGER
ICOMPQ, INFO, LDQ, LDQS, N, QSIZ
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), Q( LDQ, * ), QSTORE( LDQS, * ),
WORK( * )
.SH PURPOSE
DLAED0 computes all eigenvalues and corresponding eigenvectors of a
symmetric tridiagonal matrix using the divide and conquer method.

.SH ARGUMENTS
.TP 8
ICOMPQ  (input) INTEGER
= 0:  Compute eigenvalues only.
.br
= 1:  Compute eigenvectors of original dense symmetric matrix
also.  On entry, Q contains the orthogonal matrix used
to reduce the original matrix to tridiagonal form.
= 2:  Compute eigenvalues and eigenvectors of tridiagonal
matrix.
.TP 7
QSIZ   (input) INTEGER
The dimension of the orthogonal matrix used to reduce
the full matrix to tridiagonal form.  QSIZ >= N if ICOMPQ = 1.
.TP 7
N      (input) INTEGER
The dimension of the symmetric tridiagonal matrix.  N >= 0.
.TP 7
D      (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the main diagonal of the tridiagonal matrix.
On exit, its eigenvalues.
.TP 7
E      (input) DOUBLE PRECISION array, dimension (N-1)
The off-diagonal elements of the tridiagonal matrix.
On exit, E has been destroyed.
.TP 7
Q      (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
On entry, Q must contain an N-by-N orthogonal matrix.
If ICOMPQ = 0    Q is not referenced.
If ICOMPQ = 1    On entry, Q is a subset of the columns of the
orthogonal matrix used to reduce the full
matrix to tridiagonal form corresponding to
the subset of the full matrix which is being
decomposed at this time.
If ICOMPQ = 2    On entry, Q will be the identity matrix.
On exit, Q contains the eigenvectors of the
tridiagonal matrix.
.TP 7
LDQ    (input) INTEGER
The leading dimension of the array Q.  If eigenvectors are
desired, then  LDQ >= max(1,N).  In any case,  LDQ >= 1.

QSTORE (workspace) DOUBLE PRECISION array, dimension (LDQS, N)
Referenced only when ICOMPQ = 1.  Used to store parts of
the eigenvector matrix when the updating matrix multiplies
take place.
.TP 7
LDQS   (input) INTEGER
The leading dimension of the array QSTORE.  If ICOMPQ = 1,
then  LDQS >= max(1,N).  In any case,  LDQS >= 1.
.TP 7
WORK   (workspace) DOUBLE PRECISION array,
If ICOMPQ = 0 or 1, the dimension of WORK must be at least
1 + 3*N + 2*N*lg N + 2*N**2
( lg( N ) = smallest integer k
such that 2^k >= N )
If ICOMPQ = 2, the dimension of WORK must be at least
4*N + N**2.
.TP 7
IWORK  (workspace) INTEGER array,
If ICOMPQ = 0 or 1, the dimension of IWORK must be at least
6 + 6*N + 5*N*lg N.
( lg( N ) = smallest integer k
such that 2^k >= N )
If ICOMPQ = 2, the dimension of IWORK must be at least
3 + 5*N.
.TP 7
INFO   (output) INTEGER
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

