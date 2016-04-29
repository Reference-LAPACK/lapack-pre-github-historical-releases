.TH  CGEES 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CGEES - for an N-by-N complex nonsymmetric matrix A, the eigenvalues, the Schur form T, and, optionally, the matrix of Schur vectors Z
.SH SYNOPSIS
.TP 18
SUBROUTINE CGEES(
JOBVS, SORT, SELECT, N, A, LDA, SDIM, W, VS,
LDVS, WORK, LWORK, RWORK, BWORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBVS, SORT
.TP 18
.ti +4
INTEGER
INFO, LDA, LDVS, LWORK, N, SDIM
.TP 18
.ti +4
LOGICAL
BWORK( * )
.TP 18
.ti +4
REAL
RWORK( * )
.TP 18
.ti +4
COMPLEX
A( LDA, * ), VS( LDVS, * ), W( * ), WORK( * )
.TP 18
.ti +4
LOGICAL
SELECT
.TP 18
.ti +4
EXTERNAL
SELECT
.SH PURPOSE
CGEES computes for an N-by-N complex nonsymmetric matrix A, the
eigenvalues, the Schur form T, and, optionally, the matrix of Schur
vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).

Optionally, it also orders the eigenvalues on the diagonal of the
Schur form so that selected eigenvalues are at the top left.
The leading columns of Z then form an orthonormal basis for the
invariant subspace corresponding to the selected eigenvalues.

A complex matrix is in Schur form if it is upper triangular.

.SH ARGUMENTS
.TP 8
JOBVS   (input) CHARACTER*1
= \(aqN\(aq: Schur vectors are not computed;
.br
= \(aqV\(aq: Schur vectors are computed.
.TP 8
SORT    (input) CHARACTER*1
Specifies whether or not to order the eigenvalues on the
diagonal of the Schur form.
= \(aqN\(aq: Eigenvalues are not ordered:
.br
= \(aqS\(aq: Eigenvalues are ordered (see SELECT).
.TP 8
SELECT  (external procedure) LOGICAL FUNCTION of one COMPLEX argument
SELECT must be declared EXTERNAL in the calling subroutine.
If SORT = \(aqS\(aq, SELECT is used to select eigenvalues to order
to the top left of the Schur form.
IF SORT = \(aqN\(aq, SELECT is not referenced.
The eigenvalue W(j) is selected if SELECT(W(j)) is true.
.TP 8
N       (input) INTEGER
The order of the matrix A. N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the N-by-N matrix A.
On exit, A has been overwritten by its Schur form T.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
SDIM    (output) INTEGER
If SORT = \(aqN\(aq, SDIM = 0.
If SORT = \(aqS\(aq, SDIM = number of eigenvalues for which
SELECT is true.
.TP 8
W       (output) COMPLEX array, dimension (N)
W contains the computed eigenvalues, in the same order that
they appear on the diagonal of the output Schur form T.
.TP 8
VS      (output) COMPLEX array, dimension (LDVS,N)
If JOBVS = \(aqV\(aq, VS contains the unitary matrix Z of Schur
vectors.
If JOBVS = \(aqN\(aq, VS is not referenced.
.TP 8
LDVS    (input) INTEGER
The leading dimension of the array VS.  LDVS >= 1; if
JOBVS = \(aqV\(aq, LDVS >= N.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,2*N).
For good performance, LWORK must generally be larger.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
RWORK   (workspace) REAL array, dimension (N)
.TP 8
BWORK   (workspace) LOGICAL array, dimension (N)
Not referenced if SORT = \(aqN\(aq.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value.
.br
> 0: if INFO = i, and i is
.br
<= N:  the QR algorithm failed to compute all the
.br
eigenvalues; elements 1:ILO-1 and i+1:N of W
contain those eigenvalues which have converged;
if JOBVS = \(aqV\(aq, VS contains the matrix which
reduces A to its partially converged Schur form.
= N+1: the eigenvalues could not be reordered because
some eigenvalues were too close to separate (the
problem is very ill-conditioned);
= N+2: after reordering, roundoff changed values of
some complex eigenvalues so that leading
eigenvalues in the Schur form no longer satisfy
SELECT = .TRUE..  This could also be caused by
underflow due to scaling.
