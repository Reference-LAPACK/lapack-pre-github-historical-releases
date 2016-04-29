.TH  SLAED2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLAED2 - the two sets of eigenvalues together into a single sorted set
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAED2(
K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMDA, W,
Q2, INDX, INDXC, INDXP, COLTYP, INFO )
.TP 19
.ti +4
INTEGER
INFO, K, LDQ, N, N1
.TP 19
.ti +4
REAL
RHO
.TP 19
.ti +4
INTEGER
COLTYP( * ), INDX( * ), INDXC( * ), INDXP( * ),
INDXQ( * )
.TP 19
.ti +4
REAL
D( * ), DLAMDA( * ), Q( LDQ, * ), Q2( * ),
W( * ), Z( * )
.SH PURPOSE
SLAED2 merges the two sets of eigenvalues together into a single
sorted set.  Then it tries to deflate the size of the problem.
There are two ways in which deflation can occur:  when two or more
eigenvalues are close together or if there is a tiny entry in the
Z vector.  For each such occurrence the order of the related secular
equation problem is reduced by one.
.br

.SH ARGUMENTS
.TP 7
K      (output) INTEGER
The number of non-deflated eigenvalues, and the order of the
related secular equation. 0 <= K <=N.
.TP 7
N      (input) INTEGER
The dimension of the symmetric tridiagonal matrix.  N >= 0.
.TP 7
N1     (input) INTEGER
The location of the last eigenvalue in the leading sub-matrix.
min(1,N) <= N1 <= N/2.
.TP 7
D      (input/output) REAL array, dimension (N)
On entry, D contains the eigenvalues of the two submatrices to
be combined.
On exit, D contains the trailing (N-K) updated eigenvalues
(those which were deflated) sorted into increasing order.
.TP 7
Q      (input/output) REAL array, dimension (LDQ, N)
On entry, Q contains the eigenvectors of two submatrices in
the two square blocks with corners at (1,1), (N1,N1)
and (N1+1, N1+1), (N,N).
On exit, Q contains the trailing (N-K) updated eigenvectors
(those which were deflated) in its last N-K columns.
.TP 7
LDQ    (input) INTEGER
The leading dimension of the array Q.  LDQ >= max(1,N).
.TP 7
INDXQ  (input/output) INTEGER array, dimension (N)
The permutation which separately sorts the two sub-problems
in D into ascending order.  Note that elements in the second
half of this permutation must first have N1 added to their
values. Destroyed on exit.
.TP 7
RHO    (input/output) REAL
On entry, the off-diagonal element associated with the rank-1
cut which originally split the two submatrices which are now
being recombined.
On exit, RHO has been modified to the value required by
SLAED3.
.TP 7
Z      (input) REAL array, dimension (N)
On entry, Z contains the updating vector (the last
row of the first sub-eigenvector matrix and the first row of
the second sub-eigenvector matrix).
On exit, the contents of Z have been destroyed by the updating
process.

DLAMDA (output) REAL array, dimension (N)
A copy of the first K eigenvalues which will be used by
SLAED3 to form the secular equation.
.TP 7
W      (output) REAL array, dimension (N)
The first k values of the final deflation-altered z-vector
which will be passed to SLAED3.
.TP 7
Q2     (output) REAL array, dimension (N1**2+(N-N1)**2)
A copy of the first K eigenvectors which will be used by
SLAED3 in a matrix multiply (SGEMM) to solve for the new
eigenvectors.
.TP 7
INDX   (workspace) INTEGER array, dimension (N)
The permutation used to sort the contents of DLAMDA into
ascending order.
.TP 7
INDXC  (output) INTEGER array, dimension (N)
The permutation used to arrange the columns of the deflated
Q matrix into three groups:  the first group contains non-zero
elements only at and above N1, the second contains
non-zero elements only below N1, and the third is dense.
.TP 7
INDXP  (workspace) INTEGER array, dimension (N)
The permutation used to place deflated values of D at the end
of the array.  INDXP(1:K) points to the nondeflated D-values
.br
and INDXP(K+1:N) points to the deflated eigenvalues.

COLTYP (workspace/output) INTEGER array, dimension (N)
During execution, a label which will indicate which of the
following types a column in the Q2 matrix is:
.br
1 : non-zero in the upper half only;
.br
2 : dense;
.br
3 : non-zero in the lower half only;
.br
4 : deflated.
On exit, COLTYP(i) is the number of columns of type i,
for i=1 to 4 only.
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
Based on contributions by
.br
   Jeff Rutter, Computer Science Division, University of California
   at Berkeley, USA
.br
Modified by Francoise Tisseur, University of Tennessee.
.br

