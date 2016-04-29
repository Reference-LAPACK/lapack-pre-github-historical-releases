.TH  ZGEGS 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZGEGS - i deprecated and has been replaced by routine ZGGES
.SH SYNOPSIS
.TP 18
SUBROUTINE ZGEGS(
JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHA, BETA,
VSL, LDVSL, VSR, LDVSR, WORK, LWORK, RWORK,
INFO )
.TP 18
.ti +4
CHARACTER
JOBVSL, JOBVSR
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N
.TP 18
.ti +4
DOUBLE
PRECISION RWORK( * )
.TP 18
.ti +4
COMPLEX*16
A( LDA, * ), ALPHA( * ), B( LDB, * ),
BETA( * ), VSL( LDVSL, * ), VSR( LDVSR, * ),
WORK( * )
.SH PURPOSE
This routine is deprecated and has been replaced by routine ZGGES.

ZGEGS computes the eigenvalues, Schur form, and, optionally, the
left and or/right Schur vectors of a complex matrix pair (A,B).
Given two square matrices A and B, the generalized Schur
.br
factorization has the form
.br

.br
   A = Q*S*Z**H,  B = Q*T*Z**H
.br

.br
where Q and Z are unitary matrices and S and T are upper triangular.
The columns of Q are the left Schur vectors
.br
and the columns of Z are the right Schur vectors.
.br

.br
If only the eigenvalues of (A,B) are needed, the driver routine
ZGEGV should be used instead.  See ZGEGV for a description of the
eigenvalues of the generalized nonsymmetric eigenvalue problem
(GNEP).
.br

.SH ARGUMENTS
.TP 9
JOBVSL   (input) CHARACTER*1
= \(aqN\(aq:  do not compute the left Schur vectors;
.br
= \(aqV\(aq:  compute the left Schur vectors (returned in VSL).
.TP 9
JOBVSR   (input) CHARACTER*1
.br
= \(aqN\(aq:  do not compute the right Schur vectors;
.br
= \(aqV\(aq:  compute the right Schur vectors (returned in VSR).
.TP 8
N       (input) INTEGER
The order of the matrices A, B, VSL, and VSR.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA, N)
On entry, the matrix A.
On exit, the upper triangular matrix S from the generalized
Schur factorization.
.TP 8
LDA     (input) INTEGER
The leading dimension of A.  LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB, N)
On entry, the matrix B.
On exit, the upper triangular matrix T from the generalized
Schur factorization.
.TP 8
LDB     (input) INTEGER
The leading dimension of B.  LDB >= max(1,N).
.TP 8
ALPHA   (output) COMPLEX*16 array, dimension (N)
The complex scalars alpha that define the eigenvalues of
GNEP.  ALPHA(j) = S(j,j), the diagonal element of the Schur
form of A.
.TP 8
BETA    (output) COMPLEX*16 array, dimension (N)
The non-negative real scalars beta that define the
eigenvalues of GNEP.  BETA(j) = T(j,j), the diagonal element
of the triangular factor T.

Together, the quantities alpha = ALPHA(j) and beta = BETA(j)
represent the j-th eigenvalue of the matrix pair (A,B), in
one of the forms lambda = alpha/beta or mu = beta/alpha.
Since either lambda or mu may overflow, they should not,
in general, be computed.
.TP 8
VSL     (output) COMPLEX*16 array, dimension (LDVSL,N)
If JOBVSL = \(aqV\(aq, the matrix of left Schur vectors Q.
Not referenced if JOBVSL = \(aqN\(aq.
.TP 8
LDVSL   (input) INTEGER
The leading dimension of the matrix VSL. LDVSL >= 1, and
if JOBVSL = \(aqV\(aq, LDVSL >= N.
.TP 8
VSR     (output) COMPLEX*16 array, dimension (LDVSR,N)
If JOBVSR = \(aqV\(aq, the matrix of right Schur vectors Z.
Not referenced if JOBVSR = \(aqN\(aq.
.TP 8
LDVSR   (input) INTEGER
The leading dimension of the matrix VSR. LDVSR >= 1, and
if JOBVSR = \(aqV\(aq, LDVSR >= N.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,2*N).
For good performance, LWORK must generally be larger.
To compute the optimal value of LWORK, call ILAENV to get
blocksizes (for ZGEQRF, ZUNMQR, and CUNGQR.)  Then compute:
NB  -- MAX of the blocksizes for ZGEQRF, ZUNMQR, and CUNGQR;
the optimal LWORK is N*(NB+1).

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (3*N)
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
> N:  errors that usually indicate LAPACK problems:
.br
=N+1: error return from ZGGBAL
.br
=N+2: error return from ZGEQRF
.br
=N+3: error return from ZUNMQR
.br
=N+4: error return from ZUNGQR
.br
=N+5: error return from ZGGHRD
.br
=N+6: error return from ZHGEQZ (other than failed
iteration)
=N+7: error return from ZGGBAK (computing VSL)
.br
=N+8: error return from ZGGBAK (computing VSR)
.br
=N+9: error return from ZLASCL (various places)
