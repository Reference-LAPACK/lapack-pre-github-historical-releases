.TH  SSTEIN 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SSTEIN - the eigenvectors of a real symmetric tridiagonal matrix T corresponding to specified eigenvalues, using inverse iteration
.SH SYNOPSIS
.TP 19
SUBROUTINE SSTEIN(
N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,
IWORK, IFAIL, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDZ, M, N
.TP 19
.ti +4
INTEGER
IBLOCK( * ), IFAIL( * ), ISPLIT( * ),
IWORK( * )
.TP 19
.ti +4
REAL
D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
SSTEIN computes the eigenvectors of a real symmetric tridiagonal
matrix T corresponding to specified eigenvalues, using inverse
iteration.

The maximum number of iterations allowed for each eigenvector is
specified by an internal parameter MAXITS (currently set to 5).

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input) REAL array, dimension (N)
The n diagonal elements of the tridiagonal matrix T.
.TP 8
E       (input) REAL array, dimension (N-1)
The (n-1) subdiagonal elements of the tridiagonal matrix
T, in elements 1 to N-1.
.TP 8
M       (input) INTEGER
The number of eigenvectors to be found.  0 <= M <= N.
.TP 8
W       (input) REAL array, dimension (N)
The first M elements of W contain the eigenvalues for
which eigenvectors are to be computed.  The eigenvalues
should be grouped by split-off block and ordered from
smallest to largest within the block.  ( The output array
W from SSTEBZ with ORDER = \(aqB\(aq is expected here. )
.TP 8
IBLOCK  (input) INTEGER array, dimension (N)
The submatrix indices associated with the corresponding
eigenvalues in W; IBLOCK(i)=1 if eigenvalue W(i) belongs to
the first submatrix from the top, =2 if W(i) belongs to
the second submatrix, etc.  ( The output array IBLOCK
from SSTEBZ is expected here. )
.TP 8
ISPLIT  (input) INTEGER array, dimension (N)
The splitting points, at which T breaks up into submatrices.
The first submatrix consists of rows/columns 1 to
ISPLIT( 1 ), the second of rows/columns ISPLIT( 1 )+1
through ISPLIT( 2 ), etc.
( The output array ISPLIT from SSTEBZ is expected here. )
.TP 8
Z       (output) REAL array, dimension (LDZ, M)
The computed eigenvectors.  The eigenvector associated
with the eigenvalue W(i) is stored in the i-th column of
Z.  Any vector which fails to converge is set to its current
iterate after MAXITS iterations.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= max(1,N).
.TP 8
WORK    (workspace) REAL array, dimension (5*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
IFAIL   (output) INTEGER array, dimension (M)
On normal exit, all elements of IFAIL are zero.
If one or more eigenvectors fail to converge after
MAXITS iterations, then their indices are stored in
array IFAIL.
.TP 8
INFO    (output) INTEGER
= 0: successful exit.
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = i, then i eigenvectors failed to converge
in MAXITS iterations.  Their indices are stored in
array IFAIL.
.SH PARAMETERS
.TP 8
MAXITS  INTEGER, default = 5
The maximum number of iterations performed.
.TP 8
EXTRA   INTEGER, default = 2
The number of iterations performed after norm growth
criterion is satisfied, should be at least 1.
