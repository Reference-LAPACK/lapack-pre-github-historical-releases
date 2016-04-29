.TH  CLAED8 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CLAED8 - the two sets of eigenvalues together into a single sorted set
.SH SYNOPSIS
.TP 19
SUBROUTINE CLAED8(
K, N, QSIZ, Q, LDQ, D, RHO, CUTPNT, Z, DLAMDA,
Q2, LDQ2, W, INDXP, INDX, INDXQ, PERM, GIVPTR,
GIVCOL, GIVNUM, INFO )
.TP 19
.ti +4
INTEGER
CUTPNT, GIVPTR, INFO, K, LDQ, LDQ2, N, QSIZ
.TP 19
.ti +4
REAL
RHO
.TP 19
.ti +4
INTEGER
GIVCOL( 2, * ), INDX( * ), INDXP( * ),
INDXQ( * ), PERM( * )
.TP 19
.ti +4
REAL
D( * ), DLAMDA( * ), GIVNUM( 2, * ), W( * ),
Z( * )
.TP 19
.ti +4
COMPLEX
Q( LDQ, * ), Q2( LDQ2, * )
.SH PURPOSE
CLAED8 merges the two sets of eigenvalues together into a single
sorted set.  Then it tries to deflate the size of the problem.
There are two ways in which deflation can occur:  when two or more
eigenvalues are close together or if there is a tiny element in the
Z vector.  For each such occurrence the order of the related secular
equation problem is reduced by one.
.br

.SH ARGUMENTS
.TP 7
K      (output) INTEGER
Contains the number of non-deflated eigenvalues.
This is the order of the related secular equation.
.TP 7
N      (input) INTEGER
The dimension of the symmetric tridiagonal matrix.  N >= 0.
.TP 7
QSIZ   (input) INTEGER
The dimension of the unitary matrix used to reduce
the dense or band matrix to tridiagonal form.
QSIZ >= N if ICOMPQ = 1.
.TP 7
Q      (input/output) COMPLEX array, dimension (LDQ,N)
On entry, Q contains the eigenvectors of the partially solved
system which has been previously updated in matrix
multiplies with other partially solved eigensystems.
On exit, Q contains the trailing (N-K) updated eigenvectors
(those which were deflated) in its last N-K columns.
.TP 7
LDQ    (input) INTEGER
The leading dimension of the array Q.  LDQ >= max( 1, N ).
.TP 7
D      (input/output) REAL array, dimension (N)
On entry, D contains the eigenvalues of the two submatrices to
be combined.  On exit, D contains the trailing (N-K) updated
eigenvalues (those which were deflated) sorted into increasing
order.
.TP 7
RHO    (input/output) REAL
Contains the off diagonal element associated with the rank-1
cut which originally split the two submatrices which are now
being recombined. RHO is modified during the computation to
the value required by SLAED3.

CUTPNT (input) INTEGER
Contains the location of the last eigenvalue in the leading
sub-matrix.  MIN(1,N) <= CUTPNT <= N.
.TP 7
Z      (input) REAL array, dimension (N)
On input this vector contains the updating vector (the last
row of the first sub-eigenvector matrix and the first row of
the second sub-eigenvector matrix).  The contents of Z are
destroyed during the updating process.

DLAMDA (output) REAL array, dimension (N)
Contains a copy of the first K eigenvalues which will be used
by SLAED3 to form the secular equation.
.TP 7
Q2     (output) COMPLEX array, dimension (LDQ2,N)
If ICOMPQ = 0, Q2 is not referenced.  Otherwise,
Contains a copy of the first K eigenvectors which will be used
by SLAED7 in a matrix multiply (SGEMM) to update the new
eigenvectors.
.TP 7
LDQ2   (input) INTEGER
The leading dimension of the array Q2.  LDQ2 >= max( 1, N ).
.TP 7
W      (output) REAL array, dimension (N)
This will hold the first k values of the final
deflation-altered z-vector and will be passed to SLAED3.
.TP 7
INDXP  (workspace) INTEGER array, dimension (N)
This will contain the permutation used to place deflated
values of D at the end of the array. On output INDXP(1:K)
.br
points to the nondeflated D-values and INDXP(K+1:N)
points to the deflated eigenvalues.
.TP 7
INDX   (workspace) INTEGER array, dimension (N)
This will contain the permutation used to sort the contents of
D into ascending order.
.TP 7
INDXQ  (input) INTEGER array, dimension (N)
This contains the permutation which separately sorts the two
sub-problems in D into ascending order.  Note that elements in
the second half of this permutation must first have CUTPNT
added to their values in order to be accurate.
.TP 7
PERM   (output) INTEGER array, dimension (N)
Contains the permutations (from deflation and sorting) to be
applied to each eigenblock.

GIVPTR (output) INTEGER
Contains the number of Givens rotations which took place in
this subproblem.

GIVCOL (output) INTEGER array, dimension (2, N)
Each pair of numbers indicates a pair of columns to take place
in a Givens rotation.

GIVNUM (output) REAL array, dimension (2, N)
Each number indicates the S value to be used in the
corresponding Givens rotation.
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
