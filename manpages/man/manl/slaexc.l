.TH  SLAEXC 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAEXC - adjacent diagonal blocks T11 and T22 of order 1 or 2 in an upper quasi-triangular matrix T by an orthogonal similarity transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAEXC(
WANTQ, N, T, LDT, Q, LDQ, J1, N1, N2, WORK,
INFO )
.TP 19
.ti +4
LOGICAL
WANTQ
.TP 19
.ti +4
INTEGER
INFO, J1, LDQ, LDT, N, N1, N2
.TP 19
.ti +4
REAL
Q( LDQ, * ), T( LDT, * ), WORK( * )
.SH PURPOSE
SLAEXC swaps adjacent diagonal blocks T11 and T22 of order 1 or 2 in
an upper quasi-triangular matrix T by an orthogonal similarity
transformation.

T must be in Schur canonical form, that is, block upper triangular
with 1-by-1 and 2-by-2 diagonal blocks; each 2-by-2 diagonal block
has its diagonal elemnts equal and its off-diagonal elements of
opposite sign.
.br

.SH ARGUMENTS
.TP 8
WANTQ   (input) LOGICAL
= .TRUE. : accumulate the transformation in the matrix Q;
.br
= .FALSE.: do not accumulate the transformation.
.TP 8
N       (input) INTEGER
The order of the matrix T. N >= 0.
.TP 8
T       (input/output) REAL array, dimension (LDT,N)
On entry, the upper quasi-triangular matrix T, in Schur
canonical form.
On exit, the updated matrix T, again in Schur canonical form.
.TP 8
LDT     (input)  INTEGER
The leading dimension of the array T. LDT >= max(1,N).
.TP 8
Q       (input/output) REAL array, dimension (LDQ,N)
On entry, if WANTQ is .TRUE., the orthogonal matrix Q.
On exit, if WANTQ is .TRUE., the updated matrix Q.
If WANTQ is .FALSE., Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.
LDQ >= 1; and if WANTQ is .TRUE., LDQ >= N.
.TP 8
J1      (input) INTEGER
The index of the first row of the first block T11.
.TP 8
N1      (input) INTEGER
The order of the first block T11. N1 = 0, 1 or 2.
.TP 8
N2      (input) INTEGER
The order of the second block T22. N2 = 0, 1 or 2.
.TP 8
WORK    (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
= 1: the transformed matrix T would be too far from Schur
form; the blocks are not swapped and T and Q are
unchanged.
