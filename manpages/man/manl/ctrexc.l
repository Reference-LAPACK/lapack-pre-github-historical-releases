.TH  CTREXC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CTREXC - the Schur factorization of a complex matrix A = Q*T*Q**H, so that the diagonal element of T with row index IFST is moved to row ILST
.SH SYNOPSIS
.TP 19
SUBROUTINE CTREXC(
COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, INFO )
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
COMPLEX
Q( LDQ, * ), T( LDT, * )
.SH PURPOSE
CTREXC reorders the Schur factorization of a complex matrix
A = Q*T*Q**H, so that the diagonal element of T with row index IFST
is moved to row ILST.

The Schur form T is reordered by a unitary similarity transformation
Z**H*T*Z, and optionally the matrix Q of Schur vectors is updated by
postmultplying it with Z.
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
T       (input/output) COMPLEX array, dimension (LDT,N)
On entry, the upper triangular matrix T.
On exit, the reordered upper triangular matrix.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= max(1,N).
.TP 8
Q       (input/output) COMPLEX array, dimension (LDQ,N)
On entry, if COMPQ = \(aqV\(aq, the matrix Q of Schur vectors.
On exit, if COMPQ = \(aqV\(aq, Q has been postmultiplied by the
unitary transformation matrix Z which reorders T.
If COMPQ = \(aqN\(aq, Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  LDQ >= max(1,N).
.TP 8
IFST    (input) INTEGER
ILST    (input) INTEGER
Specify the reordering of the diagonal elements of T:
The element with row index IFST is moved to row ILST by a
sequence of transpositions between adjacent elements.
1 <= IFST <= N; 1 <= ILST <= N.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
