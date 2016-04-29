.TH  CTRSEN 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CTRSEN - the Schur factorization of a complex matrix A = Q*T*Q**H, so that a selected cluster of eigenvalues appears in the leading positions on the diagonal of the upper triangular matrix T, and the leading columns of Q form an orthonormal basis of the corresponding right invariant subspace
.SH SYNOPSIS
.TP 19
SUBROUTINE CTRSEN(
JOB, COMPQ, SELECT, N, T, LDT, Q, LDQ, W, M, S,
SEP, WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
COMPQ, JOB
.TP 19
.ti +4
INTEGER
INFO, LDQ, LDT, LWORK, M, N
.TP 19
.ti +4
REAL
S, SEP
.TP 19
.ti +4
LOGICAL
SELECT( * )
.TP 19
.ti +4
COMPLEX
Q( LDQ, * ), T( LDT, * ), W( * ), WORK( * )
.SH PURPOSE
CTRSEN reorders the Schur factorization of a complex matrix
A = Q*T*Q**H, so that a selected cluster of eigenvalues appears in
the leading positions on the diagonal of the upper triangular matrix
T, and the leading columns of Q form an orthonormal basis of the
corresponding right invariant subspace.

Optionally the routine computes the reciprocal condition numbers of
the cluster of eigenvalues and/or the invariant subspace.

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
Specifies whether condition numbers are required for the
cluster of eigenvalues (S) or the invariant subspace (SEP):
.br
= \(aqN\(aq: none;
.br
= \(aqE\(aq: for eigenvalues only (S);
.br
= \(aqV\(aq: for invariant subspace only (SEP);
.br
= \(aqB\(aq: for both eigenvalues and invariant subspace (S and
SEP).
.TP 8
COMPQ   (input) CHARACTER*1
= \(aqV\(aq: update the matrix Q of Schur vectors;
.br
= \(aqN\(aq: do not update Q.
.TP 8
SELECT  (input) LOGICAL array, dimension (N)
SELECT specifies the eigenvalues in the selected cluster. To
select the j-th eigenvalue, SELECT(j) must be set to .TRUE..
.TP 8
N       (input) INTEGER
The order of the matrix T. N >= 0.
.TP 8
T       (input/output) COMPLEX array, dimension (LDT,N)
On entry, the upper triangular matrix T.
On exit, T is overwritten by the reordered matrix T, with the
selected eigenvalues as the leading diagonal elements.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= max(1,N).
.TP 8
Q       (input/output) COMPLEX array, dimension (LDQ,N)
On entry, if COMPQ = \(aqV\(aq, the matrix Q of Schur vectors.
On exit, if COMPQ = \(aqV\(aq, Q has been postmultiplied by the
unitary transformation matrix which reorders T; the leading M
columns of Q form an orthonormal basis for the specified
invariant subspace.
If COMPQ = \(aqN\(aq, Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.
LDQ >= 1; and if COMPQ = \(aqV\(aq, LDQ >= N.
.TP 8
W       (output) COMPLEX array, dimension (N)
The reordered eigenvalues of T, in the same order as they
appear on the diagonal of T.
.TP 8
M       (output) INTEGER
The dimension of the specified invariant subspace.
0 <= M <= N.
.TP 8
S       (output) REAL
If JOB = \(aqE\(aq or \(aqB\(aq, S is a lower bound on the reciprocal
condition number for the selected cluster of eigenvalues.
S cannot underestimate the true reciprocal condition number
by more than a factor of sqrt(N). If M = 0 or N, S = 1.
If JOB = \(aqN\(aq or \(aqV\(aq, S is not referenced.
.TP 8
SEP     (output) REAL
If JOB = \(aqV\(aq or \(aqB\(aq, SEP is the estimated reciprocal
condition number of the specified invariant subspace. If
M = 0 or N, SEP = norm(T).
If JOB = \(aqN\(aq or \(aqE\(aq, SEP is not referenced.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If JOB = \(aqN\(aq, LWORK >= 1;
if JOB = \(aqE\(aq, LWORK = max(1,M*(N-M));
if JOB = \(aqV\(aq or \(aqB\(aq, LWORK >= max(1,2*M*(N-M)).

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
CTRSEN first collects the selected eigenvalues by computing a unitary
transformation Z to move them to the top left corner of T. In other
words, the selected eigenvalues are the eigenvalues of T11 in:

              Z\(aq*T*Z = ( T11 T12 ) n1
.br
                       (  0  T22 ) n2
.br
                          n1  n2
.br

where N = n1+n2 and Z\(aq means the conjugate transpose of Z. The first
n1 columns of Z span the specified invariant subspace of T.

If T has been obtained from the Schur factorization of a matrix
A = Q*T*Q\(aq, then the reordered Schur factorization of A is given by
A = (Q*Z)*(Z\(aq*T*Z)*(Q*Z)\(aq, and the first n1 columns of Q*Z span the
corresponding invariant subspace of A.
.br

The reciprocal condition number of the average of the eigenvalues of
T11 may be returned in S. S lies between 0 (very badly conditioned)
and 1 (very well conditioned). It is computed as follows. First we
compute R so that
.br

                       P = ( I  R ) n1
.br
                           ( 0  0 ) n2
.br
                             n1 n2
.br

is the projector on the invariant subspace associated with T11.
R is the solution of the Sylvester equation:
.br

                      T11*R - R*T22 = T12.
.br

Let F-norm(M) denote the Frobenius-norm of M and 2-norm(M) denote
the two-norm of M. Then S is computed as the lower bound
.br

                    (1 + F-norm(R)**2)**(-1/2)
.br

on the reciprocal of 2-norm(P), the true reciprocal condition number.
S cannot underestimate 1 / 2-norm(P) by more than a factor of
sqrt(N).
.br

An approximate error bound for the computed average of the
eigenvalues of T11 is
.br

                       EPS * norm(T) / S
.br

where EPS is the machine precision.
.br

The reciprocal condition number of the right invariant subspace
spanned by the first n1 columns of Z (or of Q*Z) is returned in SEP.
SEP is defined as the separation of T11 and T22:
.br

                   sep( T11, T22 ) = sigma-min( C )
.br

where sigma-min(C) is the smallest singular value of the
.br
n1*n2-by-n1*n2 matrix
.br

   C  = kprod( I(n2), T11 ) - kprod( transpose(T22), I(n1) )

I(m) is an m by m identity matrix, and kprod denotes the Kronecker
product. We estimate sigma-min(C) by the reciprocal of an estimate of
the 1-norm of inverse(C). The true reciprocal 1-norm of inverse(C)
cannot differ from sigma-min(C) by more than a factor of sqrt(n1*n2).

When SEP is small, small changes in T can cause large changes in
the invariant subspace. An approximate bound on the maximum angular
error in the computed right invariant subspace is
.br

                    EPS * norm(T) / SEP
.br

