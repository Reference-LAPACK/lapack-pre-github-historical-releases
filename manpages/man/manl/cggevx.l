.TH  CGGEVX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CGGEVX - for a pair of N-by-N complex nonsymmetric matrices (A,B) the generalized eigenvalues, and optionally, the left and/or right generalized eigenvectors
.SH SYNOPSIS
.TP 19
SUBROUTINE CGGEVX(
BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B, LDB,
ALPHA, BETA, VL, LDVL, VR, LDVR, ILO, IHI,
LSCALE, RSCALE, ABNRM, BBNRM, RCONDE, RCONDV,
WORK, LWORK, RWORK, IWORK, BWORK, INFO )
.TP 19
.ti +4
CHARACTER
BALANC, JOBVL, JOBVR, SENSE
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LDB, LDVL, LDVR, LWORK, N
.TP 19
.ti +4
REAL
ABNRM, BBNRM
.TP 19
.ti +4
LOGICAL
BWORK( * )
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
LSCALE( * ), RCONDE( * ), RCONDV( * ),
RSCALE( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), ALPHA( * ), B( LDB, * ),
BETA( * ), VL( LDVL, * ), VR( LDVR, * ),
WORK( * )
.SH PURPOSE
CGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
(A,B) the generalized eigenvalues, and optionally, the left and/or
right generalized eigenvectors.

Optionally, it also computes a balancing transformation to improve
the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
the eigenvalues (RCONDE), and reciprocal condition numbers for the
right eigenvectors (RCONDV).
.br

A generalized eigenvalue for a pair of matrices (A,B) is a scalar
lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
singular. It is usually represented as the pair (alpha,beta), as
there is a reasonable interpretation for beta=0, and even for both
being zero.
.br

The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
of (A,B) satisfies
.br
                 A * v(j) = lambda(j) * B * v(j) .
.br
The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
of (A,B) satisfies
.br
                 u(j)**H * A  = lambda(j) * u(j)**H * B.
.br
where u(j)**H is the conjugate-transpose of u(j).
.br


.SH ARGUMENTS
.TP 8
BALANC  (input) CHARACTER*1
Specifies the balance option to be performed:
.br
= \(aqN\(aq:  do not diagonally scale or permute;
.br
= \(aqP\(aq:  permute only;
.br
= \(aqS\(aq:  scale only;
.br
= \(aqB\(aq:  both permute and scale.
Computed reciprocal condition numbers will be for the
matrices after permuting and/or balancing. Permuting does
not change condition numbers (in exact arithmetic), but
balancing does.
.TP 8
JOBVL   (input) CHARACTER*1
= \(aqN\(aq:  do not compute the left generalized eigenvectors;
.br
= \(aqV\(aq:  compute the left generalized eigenvectors.
.TP 8
JOBVR   (input) CHARACTER*1
.br
= \(aqN\(aq:  do not compute the right generalized eigenvectors;
.br
= \(aqV\(aq:  compute the right generalized eigenvectors.
.TP 8
SENSE   (input) CHARACTER*1
Determines which reciprocal condition numbers are computed.
= \(aqN\(aq: none are computed;
.br
= \(aqE\(aq: computed for eigenvalues only;
.br
= \(aqV\(aq: computed for eigenvectors only;
.br
= \(aqB\(aq: computed for eigenvalues and eigenvectors.
.TP 8
N       (input) INTEGER
The order of the matrices A, B, VL, and VR.  N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA, N)
On entry, the matrix A in the pair (A,B).
On exit, A has been overwritten. If JOBVL=\(aqV\(aq or JOBVR=\(aqV\(aq
or both, then A contains the first part of the complex Schur
form of the "balanced" versions of the input A and B.
.TP 8
LDA     (input) INTEGER
The leading dimension of A.  LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB, N)
On entry, the matrix B in the pair (A,B).
On exit, B has been overwritten. If JOBVL=\(aqV\(aq or JOBVR=\(aqV\(aq
or both, then B contains the second part of the complex
Schur form of the "balanced" versions of the input A and B.
.TP 8
LDB     (input) INTEGER
The leading dimension of B.  LDB >= max(1,N).
.TP 8
ALPHA   (output) COMPLEX array, dimension (N)
BETA    (output) COMPLEX array, dimension (N)
On exit, ALPHA(j)/BETA(j), j=1,...,N, will be the generalized
eigenvalues.

Note: the quotient ALPHA(j)/BETA(j) ) may easily over- or
underflow, and BETA(j) may even be zero.  Thus, the user
should avoid naively computing the ratio ALPHA/BETA.
However, ALPHA will be always less than and usually
comparable with norm(A) in magnitude, and BETA always less
than and usually comparable with norm(B).
.TP 8
VL      (output) COMPLEX array, dimension (LDVL,N)
If JOBVL = \(aqV\(aq, the left generalized eigenvectors u(j) are
stored one after another in the columns of VL, in the same
order as their eigenvalues.
Each eigenvector will be scaled so the largest component
will have abs(real part) + abs(imag. part) = 1.
Not referenced if JOBVL = \(aqN\(aq.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the matrix VL. LDVL >= 1, and
if JOBVL = \(aqV\(aq, LDVL >= N.
.TP 8
VR      (output) COMPLEX array, dimension (LDVR,N)
If JOBVR = \(aqV\(aq, the right generalized eigenvectors v(j) are
stored one after another in the columns of VR, in the same
order as their eigenvalues.
Each eigenvector will be scaled so the largest component
will have abs(real part) + abs(imag. part) = 1.
Not referenced if JOBVR = \(aqN\(aq.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the matrix VR. LDVR >= 1, and
if JOBVR = \(aqV\(aq, LDVR >= N.
.TP 8
ILO     (output) INTEGER
IHI     (output) INTEGER
ILO and IHI are integer values such that on exit
A(i,j) = 0 and B(i,j) = 0 if i > j and
j = 1,...,ILO-1 or i = IHI+1,...,N.
If BALANC = \(aqN\(aq or \(aqS\(aq, ILO = 1 and IHI = N.
.TP 8
LSCALE  (output) REAL array, dimension (N)
Details of the permutations and scaling factors applied
to the left side of A and B.  If PL(j) is the index of the
row interchanged with row j, and DL(j) is the scaling
factor applied to row j, then
LSCALE(j) = PL(j)  for j = 1,...,ILO-1
= DL(j)  for j = ILO,...,IHI
= PL(j)  for j = IHI+1,...,N.
The order in which the interchanges are made is N to IHI+1,
then 1 to ILO-1.
.TP 8
RSCALE  (output) REAL array, dimension (N)
Details of the permutations and scaling factors applied
to the right side of A and B.  If PR(j) is the index of the
column interchanged with column j, and DR(j) is the scaling
factor applied to column j, then
RSCALE(j) = PR(j)  for j = 1,...,ILO-1
= DR(j)  for j = ILO,...,IHI
= PR(j)  for j = IHI+1,...,N
The order in which the interchanges are made is N to IHI+1,
then 1 to ILO-1.
.TP 8
ABNRM   (output) REAL
The one-norm of the balanced matrix A.
.TP 8
BBNRM   (output) REAL
The one-norm of the balanced matrix B.
.TP 8
RCONDE  (output) REAL array, dimension (N)
If SENSE = \(aqE\(aq or \(aqB\(aq, the reciprocal condition numbers of
the eigenvalues, stored in consecutive elements of the array.
If SENSE = \(aqN\(aq or \(aqV\(aq, RCONDE is not referenced.
.TP 8
RCONDV  (output) REAL array, dimension (N)
If SENSE = \(aqV\(aq or \(aqB\(aq, the estimated reciprocal condition
numbers of the eigenvectors, stored in consecutive elements
of the array. If the eigenvalues cannot be reordered to
compute RCONDV(j), RCONDV(j) is set to 0; this can only occur
when the true value would be very small anyway. 
If SENSE = \(aqN\(aq or \(aqE\(aq, RCONDV is not referenced.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,2*N).
If SENSE = \(aqE\(aq, LWORK >= max(1,4*N).
If SENSE = \(aqV\(aq or \(aqB\(aq, LWORK >= max(1,2*N*N+2*N).

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
RWORK   (workspace) REAL array, dimension (lrwork)
lrwork must be at least max(1,6*N) if BALANC = \(aqS\(aq or \(aqB\(aq,
and at least max(1,2*N) otherwise.
Real workspace.
.TP 8
IWORK   (workspace) INTEGER array, dimension (N+2)
If SENSE = \(aqE\(aq, IWORK is not referenced.
.TP 8
BWORK   (workspace) LOGICAL array, dimension (N)
If SENSE = \(aqN\(aq, BWORK is not referenced.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
= 1,...,N:
The QZ iteration failed.  No eigenvectors have been
calculated, but ALPHA(j) and BETA(j) should be correct
for j=INFO+1,...,N.
> N:  =N+1: other than QZ iteration failed in CHGEQZ.
.br
=N+2: error return from CTGEVC.
.SH FURTHER DETAILS
Balancing a matrix pair (A,B) includes, first, permuting rows and
columns to isolate eigenvalues, second, applying diagonal similarity
transformation to the rows and columns to make the rows and columns
as close in norm as possible. The computed reciprocal condition
numbers correspond to the balanced matrix. Permuting rows and columns
will not change the condition numbers (in exact arithmetic) but
diagonal scaling will.  For further explanation of balancing, see
section 4.11.1.2 of LAPACK Users\(aq Guide.
.br

An approximate error bound on the chordal distance between the i-th
computed generalized eigenvalue w and the corresponding exact
eigenvalue lambda is
.br

     chord(w, lambda) <= EPS * norm(ABNRM, BBNRM) / RCONDE(I)

An approximate error bound for the angle between the i-th computed
eigenvector VL(i) or VR(i) is given by
.br

     EPS * norm(ABNRM, BBNRM) / DIF(i).
.br

For further explanation of the reciprocal condition numbers RCONDE
and RCONDV, see section 4.11 of LAPACK User\(aqs Guide.
.br

