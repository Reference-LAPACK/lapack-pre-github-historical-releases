.TH  DLAEDA 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DLAEDA - the Z vector corresponding to the merge step in the CURLVLth step of the merge process with TLVLS steps for the CURPBMth problem
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAEDA(
N, TLVLS, CURLVL, CURPBM, PRMPTR, PERM, GIVPTR,
GIVCOL, GIVNUM, Q, QPTR, Z, ZTEMP, INFO )
.TP 19
.ti +4
INTEGER
CURLVL, CURPBM, INFO, N, TLVLS
.TP 19
.ti +4
INTEGER
GIVCOL( 2, * ), GIVPTR( * ), PERM( * ),
PRMPTR( * ), QPTR( * )
.TP 19
.ti +4
DOUBLE
PRECISION GIVNUM( 2, * ), Q( * ), Z( * ), ZTEMP( * )
.SH PURPOSE
DLAEDA computes the Z vector corresponding to the merge step in the
CURLVLth step of the merge process with TLVLS steps for the CURPBMth
problem.

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
The dimension of the symmetric tridiagonal matrix.  N >= 0.
.TP 7
TLVLS  (input) INTEGER
The total number of merging levels in the overall divide and
conquer tree.

CURLVL (input) INTEGER
The current level in the overall merge routine,
0 <= curlvl <= tlvls.

CURPBM (input) INTEGER
The current problem in the current level in the overall
merge routine (counting from upper left to lower right).

PRMPTR (input) INTEGER array, dimension (N lg N)
Contains a list of pointers which indicate where in PERM a
level\(aqs permutation is stored.  PRMPTR(i+1) - PRMPTR(i)
indicates the size of the permutation and incidentally the
size of the full, non-deflated problem.
.TP 7
PERM   (input) INTEGER array, dimension (N lg N)
Contains the permutations (from deflation and sorting) to be
applied to each eigenblock.

GIVPTR (input) INTEGER array, dimension (N lg N)
Contains a list of pointers which indicate where in GIVCOL a
level\(aqs Givens rotations are stored.  GIVPTR(i+1) - GIVPTR(i)
indicates the number of Givens rotations.

GIVCOL (input) INTEGER array, dimension (2, N lg N)
Each pair of numbers indicates a pair of columns to take place
in a Givens rotation.

GIVNUM (input) DOUBLE PRECISION array, dimension (2, N lg N)
Each number indicates the S value to be used in the
corresponding Givens rotation.
.TP 7
Q      (input) DOUBLE PRECISION array, dimension (N**2)
Contains the square eigenblocks from previous levels, the
starting positions for blocks are given by QPTR.
.TP 7
QPTR   (input) INTEGER array, dimension (N+2)
Contains a list of pointers which indicate where in Q an
eigenblock is stored.  SQRT( QPTR(i+1) - QPTR(i) ) indicates
the size of the block.
.TP 7
Z      (output) DOUBLE PRECISION array, dimension (N)
On output this vector contains the updating vector (the last
row of the first sub-eigenvector matrix and the first row of
the second sub-eigenvector matrix).
.TP 7
ZTEMP  (workspace) DOUBLE PRECISION array, dimension (N)
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

