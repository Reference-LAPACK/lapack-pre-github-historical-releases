.TH  SLALSA 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLALSA - an itermediate step in solving the least squares problem by computing the SVD of the coefficient matrix in compact form (The singular vectors are computed as products of simple orthorgonal matrices.)
.SH SYNOPSIS
.TP 19
SUBROUTINE SLALSA(
ICOMPQ, SMLSIZ, N, NRHS, B, LDB, BX, LDBX, U,
LDU, VT, K, DIFL, DIFR, Z, POLES, GIVPTR,
GIVCOL, LDGCOL, PERM, GIVNUM, C, S, WORK,
IWORK, INFO )
.TP 19
.ti +4
INTEGER
ICOMPQ, INFO, LDB, LDBX, LDGCOL, LDU, N, NRHS,
SMLSIZ
.TP 19
.ti +4
INTEGER
GIVCOL( LDGCOL, * ), GIVPTR( * ), IWORK( * ),
K( * ), PERM( LDGCOL, * )
.TP 19
.ti +4
REAL
B( LDB, * ), BX( LDBX, * ), C( * ),
DIFL( LDU, * ), DIFR( LDU, * ),
GIVNUM( LDU, * ), POLES( LDU, * ), S( * ),
U( LDU, * ), VT( LDU, * ), WORK( * ),
Z( LDU, * )
.SH PURPOSE
SLALSA is an itermediate step in solving the least squares problem
by computing the SVD of the coefficient matrix in compact form (The
singular vectors are computed as products of simple orthorgonal
matrices.).

If ICOMPQ = 0, SLALSA applies the inverse of the left singular vector
matrix of an upper bidiagonal matrix to the right hand side; and if
ICOMPQ = 1, SLALSA applies the right singular vector matrix to the
right hand side. The singular vector matrices were generated in
compact form by SLALSA.
.br

.SH ARGUMENTS

ICOMPQ (input) INTEGER
Specifies whether the left or the right singular vector
matrix is involved.
= 0: Left singular vector matrix
.br
= 1: Right singular vector matrix

SMLSIZ (input) INTEGER
The maximum size of the subproblems at the bottom of the
computation tree.
.TP 7
N      (input) INTEGER
The row and column dimensions of the upper bidiagonal matrix.
.TP 7
NRHS   (input) INTEGER
The number of columns of B and BX. NRHS must be at least 1.
.TP 7
B      (input/output) REAL array, dimension ( LDB, NRHS )
On input, B contains the right hand sides of the least
squares problem in rows 1 through M.
On output, B contains the solution X in rows 1 through N.
.TP 7
LDB    (input) INTEGER
The leading dimension of B in the calling subprogram.
LDB must be at least max(1,MAX( M, N ) ).
.TP 7
BX     (output) REAL array, dimension ( LDBX, NRHS )
On exit, the result of applying the left or right singular
vector matrix to B.
.TP 7
LDBX   (input) INTEGER
The leading dimension of BX.
.TP 7
U      (input) REAL array, dimension ( LDU, SMLSIZ ).
On entry, U contains the left singular vector matrices of all
subproblems at the bottom level.
.TP 7
LDU    (input) INTEGER, LDU = > N.
The leading dimension of arrays U, VT, DIFL, DIFR,
POLES, GIVNUM, and Z.
.TP 7
VT     (input) REAL array, dimension ( LDU, SMLSIZ+1 ).
On entry, VT\(aq contains the right singular vector matrices of
all subproblems at the bottom level.
.TP 7
K      (input) INTEGER array, dimension ( N ).
.TP 7
DIFL   (input) REAL array, dimension ( LDU, NLVL ).
where NLVL = INT(log_2 (N/(SMLSIZ+1))) + 1.
.TP 7
DIFR   (input) REAL array, dimension ( LDU, 2 * NLVL ).
On entry, DIFL(*, I) and DIFR(*, 2 * I -1) record
distances between singular values on the I-th level and
singular values on the (I -1)-th level, and DIFR(*, 2 * I)
record the normalizing factors of the right singular vectors
matrices of subproblems on I-th level.
.TP 7
Z      (input) REAL array, dimension ( LDU, NLVL ).
On entry, Z(1, I) contains the components of the deflation-
adjusted updating row vector for subproblems on the I-th
level.
.TP 7
POLES  (input) REAL array, dimension ( LDU, 2 * NLVL ).
On entry, POLES(*, 2 * I -1: 2 * I) contains the new and old
singular values involved in the secular equations on the I-th
level.

GIVPTR (input) INTEGER array, dimension ( N ).
On entry, GIVPTR( I ) records the number of Givens
rotations performed on the I-th problem on the computation
tree.

GIVCOL (input) INTEGER array, dimension ( LDGCOL, 2 * NLVL ).
On entry, for each I, GIVCOL(*, 2 * I - 1: 2 * I) records the
locations of Givens rotations performed on the I-th level on
the computation tree.

LDGCOL (input) INTEGER, LDGCOL = > N.
The leading dimension of arrays GIVCOL and PERM.
.TP 7
PERM   (input) INTEGER array, dimension ( LDGCOL, NLVL ).
On entry, PERM(*, I) records permutations done on the I-th
level of the computation tree.

GIVNUM (input) REAL array, dimension ( LDU, 2 * NLVL ).
On entry, GIVNUM(*, 2 *I -1 : 2 * I) records the C- and S-
values of Givens rotations performed on the I-th level on the
computation tree.
.TP 7
C      (input) REAL array, dimension ( N ).
On entry, if the I-th subproblem is not square,
C( I ) contains the C-value of a Givens rotation related to
the right null space of the I-th subproblem.
.TP 7
S      (input) REAL array, dimension ( N ).
On entry, if the I-th subproblem is not square,
S( I ) contains the S-value of a Givens rotation related to
the right null space of the I-th subproblem.
.TP 7
WORK   (workspace) REAL array.
The dimension must be at least N.
.TP 7
IWORK  (workspace) INTEGER array.
The dimension must be at least 3 * N
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

