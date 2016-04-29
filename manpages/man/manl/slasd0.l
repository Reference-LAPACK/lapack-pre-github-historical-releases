.TH  SLASD0 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLASD0 - divide and conquer approach, SLASD0 computes the singular value decomposition (SVD) of a real upper bidiagonal N-by-M matrix B with diagonal D and offdiagonal E, where M = N + SQRE
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASD0(
N, SQRE, D, E, U, LDU, VT, LDVT, SMLSIZ, IWORK,
WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDU, LDVT, N, SMLSIZ, SQRE
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
D( * ), E( * ), U( LDU, * ), VT( LDVT, * ),
WORK( * )
.SH PURPOSE
Using a divide and conquer approach, SLASD0 computes the singular
value decomposition (SVD) of a real upper bidiagonal N-by-M
matrix B with diagonal D and offdiagonal E, where M = N + SQRE.
The algorithm computes orthogonal matrices U and VT such that
B = U * S * VT. The singular values S are overwritten on D.

A related subroutine, SLASDA, computes only the singular values,
and optionally, the singular vectors in compact form.
.br

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
On entry, the row dimension of the upper bidiagonal matrix.
This is also the dimension of the main diagonal array D.
.TP 7
SQRE   (input) INTEGER
Specifies the column dimension of the bidiagonal matrix.
= 0: The bidiagonal matrix has column dimension M = N;
.br
= 1: The bidiagonal matrix has column dimension M = N+1;
.TP 7
D      (input/output) REAL array, dimension (N)
On entry D contains the main diagonal of the bidiagonal
matrix.
On exit D, if INFO = 0, contains its singular values.
.TP 7
E      (input) REAL array, dimension (M-1)
Contains the subdiagonal entries of the bidiagonal matrix.
On exit, E has been destroyed.
.TP 7
U      (output) REAL array, dimension at least (LDQ, N)
On exit, U contains the left singular vectors.
.TP 7
LDU    (input) INTEGER
On entry, leading dimension of U.
.TP 7
VT     (output) REAL array, dimension at least (LDVT, M)
On exit, VT\(aq contains the right singular vectors.
.TP 7
LDVT   (input) INTEGER
On entry, leading dimension of VT.

SMLSIZ (input) INTEGER
On entry, maximum size of the subproblems at the
bottom of the computation tree.
.TP 7
IWORK  (workspace) INTEGER array, dimension (8*N)
.TP 7
WORK   (workspace) REAL array, dimension (3*M**2+2*M)
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

