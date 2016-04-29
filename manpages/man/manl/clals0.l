.TH  CLALS0 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CLALS0 - back the multiplying factors of either the left or the right singular vector matrix of a diagonal matrix appended by a row to the right hand side matrix B in solving the least squares problem using the divide-and-conquer SVD approach
.SH SYNOPSIS
.TP 19
SUBROUTINE CLALS0(
ICOMPQ, NL, NR, SQRE, NRHS, B, LDB, BX, LDBX,
PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM,
POLES, DIFL, DIFR, Z, K, C, S, RWORK, INFO )
.TP 19
.ti +4
INTEGER
GIVPTR, ICOMPQ, INFO, K, LDB, LDBX, LDGCOL,
LDGNUM, NL, NR, NRHS, SQRE
.TP 19
.ti +4
REAL
C, S
.TP 19
.ti +4
INTEGER
GIVCOL( LDGCOL, * ), PERM( * )
.TP 19
.ti +4
REAL
DIFL( * ), DIFR( LDGNUM, * ),
GIVNUM( LDGNUM, * ), POLES( LDGNUM, * ),
RWORK( * ), Z( * )
.TP 19
.ti +4
COMPLEX
B( LDB, * ), BX( LDBX, * )
.SH PURPOSE
CLALS0 applies back the multiplying factors of either the left or the
right singular vector matrix of a diagonal matrix appended by a row
to the right hand side matrix B in solving the least squares problem
using the divide-and-conquer SVD approach.

For the left singular vector matrix, three types of orthogonal
matrices are involved:
.br

(1L) Givens rotations: the number of such rotations is GIVPTR; the
     pairs of columns/rows they were applied to are stored in GIVCOL;
     and the C- and S-values of these rotations are stored in GIVNUM.

(2L) Permutation. The (NL+1)-st row of B is to be moved to the first
     row, and for J=2:N, PERM(J)-th row of B is to be moved to the
     J-th row.
.br

(3L) The left singular vector matrix of the remaining matrix.

For the right singular vector matrix, four types of orthogonal
matrices are involved:
.br

(1R) The right singular vector matrix of the remaining matrix.

(2R) If SQRE = 1, one extra Givens rotation to generate the right
     null space.
.br

(3R) The inverse transformation of (2L).
.br

(4R) The inverse transformation of (1L).
.br

.SH ARGUMENTS

ICOMPQ (input) INTEGER
Specifies whether singular vectors are to be computed in
factored form:
.br
= 0: Left singular vector matrix.
.br
= 1: Right singular vector matrix.
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

The bidiagonal matrix has row dimension N = NL + NR + 1,
and column dimension M = N + SQRE.
.TP 7
NRHS   (input) INTEGER
The number of columns of B and BX. NRHS must be at least 1.
.TP 7
B      (input/output) COMPLEX array, dimension ( LDB, NRHS )
On input, B contains the right hand sides of the least
squares problem in rows 1 through M. On output, B contains
the solution X in rows 1 through N.
.TP 7
LDB    (input) INTEGER
The leading dimension of B. LDB must be at least
max(1,MAX( M, N ) ).
.TP 7
BX     (workspace) COMPLEX array, dimension ( LDBX, NRHS )
.TP 7
LDBX   (input) INTEGER
The leading dimension of BX.
.TP 7
PERM   (input) INTEGER array, dimension ( N )
The permutations (from deflation and sorting) applied
to the two blocks.

GIVPTR (input) INTEGER
The number of Givens rotations which took place in this
subproblem.

GIVCOL (input) INTEGER array, dimension ( LDGCOL, 2 )
Each pair of numbers indicates a pair of rows/columns
involved in a Givens rotation.

LDGCOL (input) INTEGER
The leading dimension of GIVCOL, must be at least N.

GIVNUM (input) REAL array, dimension ( LDGNUM, 2 )
Each number indicates the C or S value used in the
corresponding Givens rotation.

LDGNUM (input) INTEGER
The leading dimension of arrays DIFR, POLES and
GIVNUM, must be at least K.
.TP 7
POLES  (input) REAL array, dimension ( LDGNUM, 2 )
On entry, POLES(1:K, 1) contains the new singular
values obtained from solving the secular equation, and
POLES(1:K, 2) is an array containing the poles in the secular
equation.
.TP 7
DIFL   (input) REAL array, dimension ( K ).
On entry, DIFL(I) is the distance between I-th updated
(undeflated) singular value and the I-th (undeflated) old
singular value.
.TP 7
DIFR   (input) REAL array, dimension ( LDGNUM, 2 ).
On entry, DIFR(I, 1) contains the distances between I-th
updated (undeflated) singular value and the I+1-th
(undeflated) old singular value. And DIFR(I, 2) is the
normalizing factor for the I-th right singular vector.
.TP 7
Z      (input) REAL array, dimension ( K )
Contain the components of the deflation-adjusted updating row
vector.
.TP 7
K      (input) INTEGER
Contains the dimension of the non-deflated matrix,
This is the order of the related secular equation. 1 <= K <=N.
.TP 7
C      (input) REAL
C contains garbage if SQRE =0 and the C-value of a Givens
rotation related to the right null space if SQRE = 1.
.TP 7
S      (input) REAL
S contains garbage if SQRE =0 and the S-value of a Givens
rotation related to the right null space if SQRE = 1.
.TP 7
RWORK  (workspace) REAL array, dimension
( K*(1+NRHS) + 2*NRHS )
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Ren-Cang Li, Computer Science Division, University of
     California at Berkeley, USA
.br
   Osni Marques, LBNL/NERSC, USA
.br

