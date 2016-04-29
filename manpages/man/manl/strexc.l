.TH  STREXC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
STREXC - the real Schur factorization of a real matrix A = Q*T*Q**T, so that the diagonal block of T with row index IFST is moved to row ILST
.SH SYNOPSIS
.TP 19
SUBROUTINE STREXC(
COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, WORK,
INFO )
.TP 19
.ti +4
CHARACTER
COMPQ
.TP 19
.ti +4
INTEGER
IFST, ILST, INFO, LDQ, LDT, N
.TP 19
.ti +4
REAL
Q( LDQ, * ), T( LDT, * ), WORK( * )
.SH PURPOSE
STREXC reorders the real Schur factorization of a real matrix
A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
moved to row ILST.

The real Schur form T is reordered by an orthogonal similarity
transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
is updated by postmultiplying it with Z.
.br

T must be in Schur canonical form (as returned by SHSEQR), that is,
block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
2-by-2 diagonal block has its diagonal elements equal and its
off-diagonal elements of opposite sign.
.br

.SH ARGUMENTS
.TP 8
COMPQ   (input) CHARACTER*1
= \(aqV\(aq:  update the matrix Q of Schur vectors;
.br
= \(aqN\(aq:  do not update Q.
.TP 8
N       (input) INTEGER
The order of the matrix T. N >= 0.
.TP 8
T       (input/output) REAL array, dimension (LDT,N)
On entry, the upper quasi-triangular matrix T, in Schur
Schur canonical form.
On exit, the reordered upper quasi-triangular matrix, again
in Schur canonical form.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= max(1,N).
.TP 8
Q       (input/output) REAL array, dimension (LDQ,N)
On entry, if COMPQ = \(aqV\(aq, the matrix Q of Schur vectors.
On exit, if COMPQ = \(aqV\(aq, Q has been postmultiplied by the
orthogonal transformation matrix Z which reorders T.
If COMPQ = \(aqN\(aq, Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  LDQ >= max(1,N).
.TP 8
IFST    (input/output) INTEGER
ILST    (input/output) INTEGER
Specify the reordering of the diagonal blocks of T.
The block with row index IFST is moved to row ILST, by a
sequence of transpositions between adjacent blocks.
On exit, if IFST pointed on entry to the second row of a
2-by-2 block, it is changed to point to the first row; ILST
always points to the first row of the block in its final
position (which may differ from its input value by +1 or -1).
1 <= IFST <= N; 1 <= ILST <= N.
.TP 8
WORK    (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
= 1:  two adjacent blocks were too close to swap (the problem
is very ill-conditioned); T may have been partially
reordered, and ILST points to the first row of the
current position of the block being moved.
