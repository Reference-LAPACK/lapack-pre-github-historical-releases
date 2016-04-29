.TH  CGGES 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CGGES - for a pair of N-by-N complex nonsymmetric matrices (A,B), the generalized eigenvalues, the generalized complex Schur form (S, T), and optionally left and/or right Schur vectors (VSL and VSR)
.SH SYNOPSIS
.TP 18
SUBROUTINE CGGES(
JOBVSL, JOBVSR, SORT, SELCTG, N, A, LDA, B, LDB,
SDIM, ALPHA, BETA, VSL, LDVSL, VSR, LDVSR, WORK,
LWORK, RWORK, BWORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBVSL, JOBVSR, SORT
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N, SDIM
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
A( LDA, * ), ALPHA( * ), B( LDB, * ),
BETA( * ), VSL( LDVSL, * ), VSR( LDVSR, * ),
WORK( * )
.TP 18
.ti +4
LOGICAL
SELCTG
.TP 18
.ti +4
EXTERNAL
SELCTG
.SH PURPOSE
CGGES computes for a pair of N-by-N complex nonsymmetric matrices
(A,B), the generalized eigenvalues, the generalized complex Schur
form (S, T), and optionally left and/or right Schur vectors (VSL
and VSR). This gives the generalized Schur factorization

        (A,B) = ( (VSL)*S*(VSR)**H, (VSL)*T*(VSR)**H )
.br

where (VSR)**H is the conjugate-transpose of VSR.
.br

Optionally, it also orders the eigenvalues so that a selected cluster
of eigenvalues appears in the leading diagonal blocks of the upper
triangular matrix S and the upper triangular matrix T. The leading
columns of VSL and VSR then form an unitary basis for the
corresponding left and right eigenspaces (deflating subspaces).

(If only the generalized eigenvalues are needed, use the driver
CGGEV instead, which is faster.)
.br

A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
usually represented as the pair (alpha,beta), as there is a
reasonable interpretation for beta=0, and even for both being zero.

A pair of matrices (S,T) is in generalized complex Schur form if S
and T are upper triangular and, in addition, the diagonal elements
of T are non-negative real numbers.
.br

.SH ARGUMENTS
.TP 8
JOBVSL  (input) CHARACTER*1
= \(aqN\(aq:  do not compute the left Schur vectors;
.br
= \(aqV\(aq:  compute the left Schur vectors.
.TP 8
JOBVSR  (input) CHARACTER*1
.br
= \(aqN\(aq:  do not compute the right Schur vectors;
.br
= \(aqV\(aq:  compute the right Schur vectors.
.TP 8
SORT    (input) CHARACTER*1
Specifies whether or not to order the eigenvalues on the
diagonal of the generalized Schur form.
= \(aqN\(aq:  Eigenvalues are not ordered;
.br
= \(aqS\(aq:  Eigenvalues are ordered (see SELCTG).
.TP 8
SELCTG  (external procedure) LOGICAL FUNCTION of two COMPLEX arguments
SELCTG must be declared EXTERNAL in the calling subroutine.
If SORT = \(aqN\(aq, SELCTG is not referenced.
If SORT = \(aqS\(aq, SELCTG is used to select eigenvalues to sort
to the top left of the Schur form.
An eigenvalue ALPHA(j)/BETA(j) is selected if
SELCTG(ALPHA(j),BETA(j)) is true.

Note that a selected complex eigenvalue may no longer satisfy
SELCTG(ALPHA(j),BETA(j)) = .TRUE. after ordering, since
ordering may change the value of complex eigenvalues
(especially if the eigenvalue is ill-conditioned), in this
case INFO is set to N+2 (See INFO below).
.TP 8
N       (input) INTEGER
The order of the matrices A, B, VSL, and VSR.  N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA, N)
On entry, the first of the pair of matrices.
On exit, A has been overwritten by its generalized Schur
form S.
.TP 8
LDA     (input) INTEGER
The leading dimension of A.  LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB, N)
On entry, the second of the pair of matrices.
On exit, B has been overwritten by its generalized Schur
form T.
.TP 8
LDB     (input) INTEGER
The leading dimension of B.  LDB >= max(1,N).
.TP 8
SDIM    (output) INTEGER
If SORT = \(aqN\(aq, SDIM = 0.
If SORT = \(aqS\(aq, SDIM = number of eigenvalues (after sorting)
for which SELCTG is true.
.TP 8
ALPHA   (output) COMPLEX array, dimension (N)
BETA    (output) COMPLEX array, dimension (N)
On exit,  ALPHA(j)/BETA(j), j=1,...,N, will be the
generalized eigenvalues.  ALPHA(j), j=1,...,N  and  BETA(j),
j=1,...,N  are the diagonals of the complex Schur form (A,B)
output by CGGES. The  BETA(j) will be non-negative real.

Note: the quotients ALPHA(j)/BETA(j) may easily over- or
underflow, and BETA(j) may even be zero.  Thus, the user
should avoid naively computing the ratio alpha/beta.
However, ALPHA will be always less than and usually
comparable with norm(A) in magnitude, and BETA always less
than and usually comparable with norm(B).
.TP 8
VSL     (output) COMPLEX array, dimension (LDVSL,N)
If JOBVSL = \(aqV\(aq, VSL will contain the left Schur vectors.
Not referenced if JOBVSL = \(aqN\(aq.
.TP 8
LDVSL   (input) INTEGER
The leading dimension of the matrix VSL. LDVSL >= 1, and
if JOBVSL = \(aqV\(aq, LDVSL >= N.
.TP 8
VSR     (output) COMPLEX array, dimension (LDVSR,N)
If JOBVSR = \(aqV\(aq, VSR will contain the right Schur vectors.
Not referenced if JOBVSR = \(aqN\(aq.
.TP 8
LDVSR   (input) INTEGER
The leading dimension of the matrix VSR. LDVSR >= 1, and
if JOBVSR = \(aqV\(aq, LDVSR >= N.
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
RWORK   (workspace) REAL array, dimension (8*N)
.TP 8
BWORK   (workspace) LOGICAL array, dimension (N)
Not referenced if SORT = \(aqN\(aq.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
=1,...,N:
The QZ iteration failed.  (A,B) are not in Schur
form, but ALPHA(j) and BETA(j) should be correct for
j=INFO+1,...,N.
> N:  =N+1: other than QZ iteration failed in CHGEQZ
.br
=N+2: after reordering, roundoff changed values of
some complex eigenvalues so that leading
eigenvalues in the Generalized Schur form no
longer satisfy SELCTG=.TRUE.  This could also
be caused due to scaling.
=N+3: reordering falied in CTGSEN.
