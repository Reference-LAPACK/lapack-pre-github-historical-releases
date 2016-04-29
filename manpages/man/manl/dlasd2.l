.TH  DLASD2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASD2 - the two sets of singular values together into a single sorted set
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASD2(
NL, NR, SQRE, K, D, Z, ALPHA, BETA, U, LDU, VT,
LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, IDXP, IDX,
IDXC, IDXQ, COLTYP, INFO )
.TP 19
.ti +4
INTEGER
INFO, K, LDU, LDU2, LDVT, LDVT2, NL, NR, SQRE
.TP 19
.ti +4
DOUBLE
PRECISION ALPHA, BETA
.TP 19
.ti +4
INTEGER
COLTYP( * ), IDX( * ), IDXC( * ), IDXP( * ),
IDXQ( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), DSIGMA( * ), U( LDU, * ),
U2( LDU2, * ), VT( LDVT, * ), VT2( LDVT2, * ),
Z( * )
.SH PURPOSE
DLASD2 merges the two sets of singular values together into a single
sorted set.  Then it tries to deflate the size of the problem.
There are two ways in which deflation can occur:  when two or more
singular values are close together or if there is a tiny entry in the
Z vector.  For each such occurrence the order of the related secular
equation problem is reduced by one.
.br

DLASD2 is called from DLASD1.
.br

.SH ARGUMENTS
.TP 7
NL     (input) INTEGER
The row dimension of the upper block.  NL >= 1.
.TP 7
NR     (input) INTEGER
The row dimension of the lower block.  NR >= 1.
.TP 7
SQRE   (input) INTEGER
= 0: the lower block is an NR-by-NR square matrix.
.br
= 1: the lower block is an NR-by-(NR+1) rectangular matrix.

The bidiagonal matrix has N = NL + NR + 1 rows and
M = N + SQRE >= N columns.
.TP 7
K      (output) INTEGER
Contains the dimension of the non-deflated matrix,
This is the order of the related secular equation. 1 <= K <=N.
.TP 7
D      (input/output) DOUBLE PRECISION array, dimension(N)
On entry D contains the singular values of the two submatrices
to be combined.  On exit D contains the trailing (N-K) updated
singular values (those which were deflated) sorted into
increasing order.
.TP 7
Z      (output) DOUBLE PRECISION array, dimension(N)
On exit Z contains the updating row vector in the secular
equation.
.TP 7
ALPHA  (input) DOUBLE PRECISION
Contains the diagonal element associated with the added row.
.TP 7
BETA   (input) DOUBLE PRECISION
Contains the off-diagonal element associated with the added
row.
.TP 7
U      (input/output) DOUBLE PRECISION array, dimension(LDU,N)
On entry U contains the left singular vectors of two
submatrices in the two square blocks with corners at (1,1),
(NL, NL), and (NL+2, NL+2), (N,N).
On exit U contains the trailing (N-K) updated left singular
vectors (those which were deflated) in its last N-K columns.
.TP 7
LDU    (input) INTEGER
The leading dimension of the array U.  LDU >= N.
.TP 7
VT     (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
On entry VT\(aq contains the right singular vectors of two
submatrices in the two square blocks with corners at (1,1),
(NL+1, NL+1), and (NL+2, NL+2), (M,M).
On exit VT\(aq contains the trailing (N-K) updated right singular
vectors (those which were deflated) in its last N-K columns.
In case SQRE =1, the last row of VT spans the right null
space.
.TP 7
LDVT   (input) INTEGER
The leading dimension of the array VT.  LDVT >= M.

DSIGMA (output) DOUBLE PRECISION array, dimension (N)
Contains a copy of the diagonal elements (K-1 singular values
and one zero) in the secular equation.
.TP 7
U2     (output) DOUBLE PRECISION array, dimension(LDU2,N)
Contains a copy of the first K-1 left singular vectors which
will be used by DLASD3 in a matrix multiply (DGEMM) to solve
for the new left singular vectors. U2 is arranged into four
blocks. The first block contains a column with 1 at NL+1 and
zero everywhere else; the second block contains non-zero
entries only at and above NL; the third contains non-zero
entries only below NL+1; and the fourth is dense.
.TP 7
LDU2   (input) INTEGER
The leading dimension of the array U2.  LDU2 >= N.
.TP 7
VT2    (output) DOUBLE PRECISION array, dimension(LDVT2,N)
VT2\(aq contains a copy of the first K right singular vectors
which will be used by DLASD3 in a matrix multiply (DGEMM) to
solve for the new right singular vectors. VT2 is arranged into
three blocks. The first block contains a row that corresponds
to the special 0 diagonal element in SIGMA; the second block
contains non-zeros only at and before NL +1; the third block
contains non-zeros only at and after  NL +2.
.TP 7
LDVT2  (input) INTEGER
The leading dimension of the array VT2.  LDVT2 >= M.
.TP 7
IDXP   (workspace) INTEGER array dimension(N)
This will contain the permutation used to place deflated
values of D at the end of the array. On output IDXP(2:K)
.br
points to the nondeflated D-values and IDXP(K+1:N)
points to the deflated singular values.
.TP 7
IDX    (workspace) INTEGER array dimension(N)
This will contain the permutation used to sort the contents of
D into ascending order.
.TP 7
IDXC   (output) INTEGER array dimension(N)
This will contain the permutation used to arrange the columns
of the deflated U matrix into three groups:  the first group
contains non-zero entries only at and above NL, the second
contains non-zero entries only below NL+2, and the third is
dense.
.TP 7
IDXQ   (input/output) INTEGER array dimension(N)
This contains the permutation which separately sorts the two
sub-problems in D into ascending order.  Note that entries in
the first hlaf of this permutation must first be moved one
position backward; and entries in the second half
must first have NL+1 added to their values.

COLTYP (workspace/output) INTEGER array dimension(N)
As workspace, this will contain a label which will indicate
which of the following types a column in the U2 matrix or a
row in the VT2 matrix is:
.br
1 : non-zero in the upper half only
.br
2 : non-zero in the lower half only
.br
3 : dense
.br
4 : deflated

On exit, it is an array of dimension 4, with COLTYP(I) being
the dimension of the I-th type columns.
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Huan Ren, Computer Science Division, University of
   California at Berkeley, USA
.br

