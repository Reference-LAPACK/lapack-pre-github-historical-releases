.TH  SLASDA 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLASDA - divide and conquer approach, SLASDA computes the singular value decomposition (SVD) of a real upper bidiagonal N-by-M matrix B with diagonal D and offdiagonal E, where M = N + SQRE
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASDA(
ICOMPQ, SMLSIZ, N, SQRE, D, E, U, LDU, VT, K,
DIFL, DIFR, Z, POLES, GIVPTR, GIVCOL, LDGCOL,
PERM, GIVNUM, C, S, WORK, IWORK, INFO )
.TP 19
.ti +4
INTEGER
ICOMPQ, INFO, LDGCOL, LDU, N, SMLSIZ, SQRE
.TP 19
.ti +4
INTEGER
GIVCOL( LDGCOL, * ), GIVPTR( * ), IWORK( * ),
K( * ), PERM( LDGCOL, * )
.TP 19
.ti +4
REAL
C( * ), D( * ), DIFL( LDU, * ), DIFR( LDU, * ),
E( * ), GIVNUM( LDU, * ), POLES( LDU, * ),
S( * ), U( LDU, * ), VT( LDU, * ), WORK( * ),
Z( LDU, * )
.SH PURPOSE
Using a divide and conquer approach, SLASDA computes the singular
value decomposition (SVD) of a real upper bidiagonal N-by-M matrix
B with diagonal D and offdiagonal E, where M = N + SQRE. The
algorithm computes the singular values in the SVD B = U * S * VT.
The orthogonal matrices U and VT are optionally computed in
compact form.
.br

A related subroutine, SLASD0, computes the singular values and
the singular vectors in explicit form.
.br

.SH ARGUMENTS

ICOMPQ (input) INTEGER
Specifies whether singular vectors are to be computed
in compact form, as follows
= 0: Compute singular values only.
.br
= 1: Compute singular vectors of upper bidiagonal
matrix in compact form.

SMLSIZ (input) INTEGER
The maximum size of the subproblems at the bottom of the
computation tree.
.TP 7
N      (input) INTEGER
The row dimension of the upper bidiagonal matrix. This is
also the dimension of the main diagonal array D.
.TP 7
SQRE   (input) INTEGER
Specifies the column dimension of the bidiagonal matrix.
= 0: The bidiagonal matrix has column dimension M = N;
.br
= 1: The bidiagonal matrix has column dimension M = N + 1.
.TP 7
D      (input/output) REAL array, dimension ( N )
On entry D contains the main diagonal of the bidiagonal
matrix. On exit D, if INFO = 0, contains its singular values.
.TP 7
E      (input) REAL array, dimension ( M-1 )
Contains the subdiagonal entries of the bidiagonal matrix.
On exit, E has been destroyed.
.TP 7
U      (output) REAL array,
dimension ( LDU, SMLSIZ ) if ICOMPQ = 1, and not referenced
if ICOMPQ = 0. If ICOMPQ = 1, on exit, U contains the left
singular vector matrices of all subproblems at the bottom
level.
.TP 7
LDU    (input) INTEGER, LDU = > N.
The leading dimension of arrays U, VT, DIFL, DIFR, POLES,
GIVNUM, and Z.
.TP 7
VT     (output) REAL array,
dimension ( LDU, SMLSIZ+1 ) if ICOMPQ = 1, and not referenced
if ICOMPQ = 0. If ICOMPQ = 1, on exit, VT\(aq contains the right
singular vector matrices of all subproblems at the bottom
level.
.TP 7
K      (output) INTEGER array, dimension ( N ) 
if ICOMPQ = 1 and dimension 1 if ICOMPQ = 0.
If ICOMPQ = 1, on exit, K(I) is the dimension of the I-th
secular equation on the computation tree.
.TP 7
DIFL   (output) REAL array, dimension ( LDU, NLVL ),
where NLVL = floor(log_2 (N/SMLSIZ))).
.TP 7
DIFR   (output) REAL array,
dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1 and
dimension ( N ) if ICOMPQ = 0.
If ICOMPQ = 1, on exit, DIFL(1:N, I) and DIFR(1:N, 2 * I - 1)
record distances between singular values on the I-th
level and singular values on the (I -1)-th level, and
DIFR(1:N, 2 * I ) contains the normalizing factors for
the right singular vector matrix. See SLASD8 for details.
.TP 7
Z      (output) REAL array,
dimension ( LDU, NLVL ) if ICOMPQ = 1 and
dimension ( N ) if ICOMPQ = 0.
The first K elements of Z(1, I) contain the components of
the deflation-adjusted updating row vector for subproblems
on the I-th level.
.TP 7
POLES  (output) REAL array,
dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1, and not referenced
if ICOMPQ = 0. If ICOMPQ = 1, on exit, POLES(1, 2*I - 1) and
POLES(1, 2*I) contain  the new and old singular values
involved in the secular equations on the I-th level.

GIVPTR (output) INTEGER array,
dimension ( N ) if ICOMPQ = 1, and not referenced if
ICOMPQ = 0. If ICOMPQ = 1, on exit, GIVPTR( I ) records
the number of Givens rotations performed on the I-th
problem on the computation tree.

GIVCOL (output) INTEGER array,
dimension ( LDGCOL, 2 * NLVL ) if ICOMPQ = 1, and not
referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
GIVCOL(1, 2 *I - 1) and GIVCOL(1, 2 *I) record the locations
of Givens rotations performed on the I-th level on the
computation tree.

LDGCOL (input) INTEGER, LDGCOL = > N.
The leading dimension of arrays GIVCOL and PERM.
.TP 7
PERM   (output) INTEGER array, dimension ( LDGCOL, NLVL ) 
if ICOMPQ = 1, and not referenced
if ICOMPQ = 0. If ICOMPQ = 1, on exit, PERM(1, I) records
permutations done on the I-th level of the computation tree.

GIVNUM (output) REAL array,
dimension ( LDU,  2 * NLVL ) if ICOMPQ = 1, and not
referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
GIVNUM(1, 2 *I - 1) and GIVNUM(1, 2 *I) record the C- and S-
values of Givens rotations performed on the I-th level on
the computation tree.
.TP 7
C      (output) REAL array,
dimension ( N ) if ICOMPQ = 1, and dimension 1 if ICOMPQ = 0.
If ICOMPQ = 1 and the I-th subproblem is not square, on exit,
C( I ) contains the C-value of a Givens rotation related to
the right null space of the I-th subproblem.
.TP 7
S      (output) REAL array, dimension ( N ) if
ICOMPQ = 1, and dimension 1 if ICOMPQ = 0. If ICOMPQ = 1
and the I-th subproblem is not square, on exit, S( I )
contains the S-value of a Givens rotation related to
the right null space of the I-th subproblem.
.TP 7
WORK   (workspace) REAL array, dimension
(6 * N + (SMLSIZ + 1)*(SMLSIZ + 1)).
.TP 7
IWORK  (workspace) INTEGER array, dimension (7*N).
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = 1, an singular value did not converge
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Huan Ren, Computer Science Division, University of
   California at Berkeley, USA
.br

