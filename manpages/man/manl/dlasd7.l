.TH  DLASD7 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASD7 - the two sets of singular values together into a single sorted set
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASD7(
ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL,
VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ,
PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM,
C, S, INFO )
.TP 19
.ti +4
INTEGER
GIVPTR, ICOMPQ, INFO, K, LDGCOL, LDGNUM, NL,
NR, SQRE
.TP 19
.ti +4
DOUBLE
PRECISION ALPHA, BETA, C, S
.TP 19
.ti +4
INTEGER
GIVCOL( LDGCOL, * ), IDX( * ), IDXP( * ),
IDXQ( * ), PERM( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), DSIGMA( * ), GIVNUM( LDGNUM, * ),
VF( * ), VFW( * ), VL( * ), VLW( * ), Z( * ),
ZW( * )
.SH PURPOSE
DLASD7 merges the two sets of singular values together into a single
sorted set. Then it tries to deflate the size of the problem. There
are two ways in which deflation can occur:  when two or more singular
values are close together or if there is a tiny entry in the Z
vector. For each such occurrence the order of the related
secular equation problem is reduced by one.
.br

DLASD7 is called from DLASD6.
.br

.SH ARGUMENTS
.TP 8
ICOMPQ  (input) INTEGER
Specifies whether singular vectors are to be computed
in compact form, as follows:
.br
= 0: Compute singular values only.
.br
= 1: Compute singular vectors of upper
bidiagonal matrix in compact form.
.TP 7
NL     (input) INTEGER
The row dimension of the upper block. NL >= 1.
.TP 7
NR     (input) INTEGER
The row dimension of the lower block. NR >= 1.
.TP 7
SQRE   (input) INTEGER
= 0: the lower block is an NR-by-NR square matrix.
.br
= 1: the lower block is an NR-by-(NR+1) rectangular matrix.

The bidiagonal matrix has
N = NL + NR + 1 rows and
M = N + SQRE >= N columns.
.TP 7
K      (output) INTEGER
Contains the dimension of the non-deflated matrix, this is
the order of the related secular equation. 1 <= K <=N.
.TP 7
D      (input/output) DOUBLE PRECISION array, dimension ( N )
On entry D contains the singular values of the two submatrices
to be combined. On exit D contains the trailing (N-K) updated
singular values (those which were deflated) sorted into
increasing order.
.TP 7
Z      (output) DOUBLE PRECISION array, dimension ( M )
On exit Z contains the updating row vector in the secular
equation.
.TP 7
ZW     (workspace) DOUBLE PRECISION array, dimension ( M )
Workspace for Z.
.TP 7
VF     (input/output) DOUBLE PRECISION array, dimension ( M )
On entry, VF(1:NL+1) contains the first components of all
.br
right singular vectors of the upper block; and VF(NL+2:M)
contains the first components of all right singular vectors
of the lower block. On exit, VF contains the first components
of all right singular vectors of the bidiagonal matrix.
.TP 7
VFW    (workspace) DOUBLE PRECISION array, dimension ( M )
Workspace for VF.
.TP 7
VL     (input/output) DOUBLE PRECISION array, dimension ( M )
On entry, VL(1:NL+1) contains the  last components of all
.br
right singular vectors of the upper block; and VL(NL+2:M)
contains the last components of all right singular vectors
of the lower block. On exit, VL contains the last components
of all right singular vectors of the bidiagonal matrix.
.TP 7
VLW    (workspace) DOUBLE PRECISION array, dimension ( M )
Workspace for VL.
.TP 7
ALPHA  (input) DOUBLE PRECISION
Contains the diagonal element associated with the added row.
.TP 7
BETA   (input) DOUBLE PRECISION
Contains the off-diagonal element associated with the added
row.

DSIGMA (output) DOUBLE PRECISION array, dimension ( N )
Contains a copy of the diagonal elements (K-1 singular values
and one zero) in the secular equation.
.TP 7
IDX    (workspace) INTEGER array, dimension ( N )
This will contain the permutation used to sort the contents of
D into ascending order.
.TP 7
IDXP   (workspace) INTEGER array, dimension ( N )
This will contain the permutation used to place deflated
values of D at the end of the array. On output IDXP(2:K)
.br
points to the nondeflated D-values and IDXP(K+1:N)
points to the deflated singular values.
.TP 7
IDXQ   (input) INTEGER array, dimension ( N )
This contains the permutation which separately sorts the two
sub-problems in D into ascending order.  Note that entries in
the first half of this permutation must first be moved one
position backward; and entries in the second half
must first have NL+1 added to their values.
.TP 7
PERM   (output) INTEGER array, dimension ( N )
The permutations (from deflation and sorting) to be applied
to each singular block. Not referenced if ICOMPQ = 0.

GIVPTR (output) INTEGER
The number of Givens rotations which took place in this
subproblem. Not referenced if ICOMPQ = 0.

GIVCOL (output) INTEGER array, dimension ( LDGCOL, 2 )
Each pair of numbers indicates a pair of columns to take place
in a Givens rotation. Not referenced if ICOMPQ = 0.

LDGCOL (input) INTEGER
The leading dimension of GIVCOL, must be at least N.

GIVNUM (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
Each number indicates the C or S value to be used in the
corresponding Givens rotation. Not referenced if ICOMPQ = 0.

LDGNUM (input) INTEGER
The leading dimension of GIVNUM, must be at least N.
.TP 7
C      (output) DOUBLE PRECISION
C contains garbage if SQRE =0 and the C-value of a Givens
rotation related to the right null space if SQRE = 1.
.TP 7
S      (output) DOUBLE PRECISION
S contains garbage if SQRE =0 and the S-value of a Givens
rotation related to the right null space if SQRE = 1.
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

