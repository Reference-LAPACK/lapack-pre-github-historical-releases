.TH  CLAED0 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CLAED0 - divide and conquer method, CLAED0 computes all eigenvalues of a symmetric tridiagonal matrix which is one diagonal block of those from reducing a dense or band Hermitian matrix and corresponding eigenvectors of the dense or band matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE CLAED0(
QSIZ, N, D, E, Q, LDQ, QSTORE, LDQS, RWORK,
IWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDQ, LDQS, N, QSIZ
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
Q( LDQ, * ), QSTORE( LDQS, * )
.SH PURPOSE
Using the divide and conquer method, CLAED0 computes all eigenvalues
of a symmetric tridiagonal matrix which is one diagonal block of
those from reducing a dense or band Hermitian matrix and
corresponding eigenvectors of the dense or band matrix.

.SH ARGUMENTS
.TP 7
QSIZ   (input) INTEGER
The dimension of the unitary matrix used to reduce
the full matrix to tridiagonal form.  QSIZ >= N if ICOMPQ = 1.
.TP 7
N      (input) INTEGER
The dimension of the symmetric tridiagonal matrix.  N >= 0.
.TP 7
D      (input/output) REAL array, dimension (N)
On entry, the diagonal elements of the tridiagonal matrix.
On exit, the eigenvalues in ascending order.
.TP 7
E      (input/output) REAL array, dimension (N-1)
On entry, the off-diagonal elements of the tridiagonal matrix.
On exit, E has been destroyed.
.TP 7
Q      (input/output) COMPLEX array, dimension (LDQ,N)
On entry, Q must contain an QSIZ x N matrix whose columns
unitarily orthonormal. It is a part of the unitary matrix
that reduces the full dense Hermitian matrix to a
(reducible) symmetric tridiagonal matrix.
.TP 7
LDQ    (input) INTEGER
The leading dimension of the array Q.  LDQ >= max(1,N).
.TP 7
IWORK  (workspace) INTEGER array,
the dimension of IWORK must be at least
6 + 6*N + 5*N*lg N
( lg( N ) = smallest integer k
such that 2^k >= N )
.TP 7
RWORK  (workspace) REAL array,
dimension (1 + 3*N + 2*N*lg N + 3*N**2)
( lg( N ) = smallest integer k
such that 2^k >= N )

QSTORE (workspace) COMPLEX array, dimension (LDQS, N)
Used to store parts of
the eigenvector matrix when the updating matrix multiplies
take place.
.TP 7
LDQS   (input) INTEGER
The leading dimension of the array QSTORE.
LDQS >= max(1,N).
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  The algorithm failed to compute an eigenvalue while
working on the submatrix lying in rows and columns
INFO/(N+1) through mod(INFO,N+1).
