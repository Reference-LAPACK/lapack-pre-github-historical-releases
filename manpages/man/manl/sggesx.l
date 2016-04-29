.TH  SGGESX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
SGGESX - for a pair of N-by-N real nonsymmetric matrices (A,B), the generalized eigenvalues, the real Schur form (S,T), and,
.SH SYNOPSIS
.TP 19
SUBROUTINE SGGESX(
JOBVSL, JOBVSR, SORT, SELCTG, SENSE, N, A, LDA,
B, LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, LDVSL,
VSR, LDVSR, RCONDE, RCONDV, WORK, LWORK, IWORK,
LIWORK, BWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBVSL, JOBVSR, SENSE, SORT
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LDVSL, LDVSR, LIWORK, LWORK, N,
SDIM
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
A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
B( LDB, * ), BETA( * ), RCONDE( 2 ),
RCONDV( 2 ), VSL( LDVSL, * ), VSR( LDVSR, * ),
WORK( * )
.TP 19
.ti +4
LOGICAL
SELCTG
.TP 19
.ti +4
EXTERNAL
SELCTG
.SH PURPOSE
SGGESX computes for a pair of N-by-N real nonsymmetric matrices
(A,B), the generalized eigenvalues, the real Schur form (S,T), and,
optionally, the left and/or right matrices of Schur vectors (VSL and
VSR).  This gives the generalized Schur factorization
.br

     (A,B) = ( (VSL) S (VSR)**T, (VSL) T (VSR)**T )
.br

Optionally, it also orders the eigenvalues so that a selected cluster
of eigenvalues appears in the leading diagonal blocks of the upper
quasi-triangular matrix S and the upper triangular matrix T; computes
a reciprocal condition number for the average of the selected
eigenvalues (RCONDE); and computes a reciprocal condition number for
the right and left deflating subspaces corresponding to the selected
eigenvalues (RCONDV). The leading columns of VSL and VSR then form
an orthonormal basis for the corresponding left and right eigenspaces
(deflating subspaces).
.br

A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
usually represented as the pair (alpha,beta), as there is a
reasonable interpretation for beta=0 or for both being zero.

A pair of matrices (S,T) is in generalized real Schur form if T is
upper triangular with non-negative diagonal and S is block upper
triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
to real generalized eigenvalues, while 2-by-2 blocks of S will be
"standardized" by making the corresponding elements of T have the
form:
.br
        [  a  0  ]
.br
        [  0  b  ]
.br

and the pair of corresponding 2-by-2 blocks in S and T will have a
complex conjugate pair of generalized eigenvalues.
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
SELCTG  (external procedure) LOGICAL FUNCTION of three REAL arguments
SELCTG must be declared EXTERNAL in the calling subroutine.
If SORT = \(aqN\(aq, SELCTG is not referenced.
If SORT = \(aqS\(aq, SELCTG is used to select eigenvalues to sort
to the top left of the Schur form.
An eigenvalue (ALPHAR(j)+ALPHAI(j))/BETA(j) is selected if
SELCTG(ALPHAR(j),ALPHAI(j),BETA(j)) is true; i.e. if either
one of a complex conjugate pair of eigenvalues is selected,
then both complex eigenvalues are selected.
Note that a selected complex eigenvalue may no longer satisfy
SELCTG(ALPHAR(j),ALPHAI(j),BETA(j)) = .TRUE. after ordering,
since ordering may change the value of complex eigenvalues
(especially if the eigenvalue is ill-conditioned), in this
case INFO is set to N+3.
.TP 8
SENSE   (input) CHARACTER*1
Determines which reciprocal condition numbers are computed.
= \(aqN\(aq : None are computed;
.br
= \(aqE\(aq : Computed for average of selected eigenvalues only;
.br
= \(aqV\(aq : Computed for selected deflating subspaces only;
.br
= \(aqB\(aq : Computed for both.
If SENSE = \(aqE\(aq, \(aqV\(aq, or \(aqB\(aq, SORT must equal \(aqS\(aq.
.TP 8
N       (input) INTEGER
The order of the matrices A, B, VSL, and VSR.  N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA, N)
On entry, the first of the pair of matrices.
On exit, A has been overwritten by its generalized Schur
form S.
.TP 8
LDA     (input) INTEGER
The leading dimension of A.  LDA >= max(1,N).
.TP 8
B       (input/output) REAL array, dimension (LDB, N)
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
for which SELCTG is true.  (Complex conjugate pairs for which
SELCTG is true for either eigenvalue count as 2.)
.TP 8
ALPHAR  (output) REAL array, dimension (N)
ALPHAI  (output) REAL array, dimension (N)
BETA    (output) REAL array, dimension (N)
On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
be the generalized eigenvalues.  ALPHAR(j) + ALPHAI(j)*i
and BETA(j),j=1,...,N  are the diagonals of the complex Schur
form (S,T) that would result if the 2-by-2 diagonal blocks of
the real Schur form of (A,B) were further reduced to
triangular form using 2-by-2 complex unitary transformations.
If ALPHAI(j) is zero, then the j-th eigenvalue is real; if
positive, then the j-th and (j+1)-st eigenvalues are a
complex conjugate pair, with ALPHAI(j+1) negative.

Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
may easily over- or underflow, and BETA(j) may even be zero.
Thus, the user should avoid naively computing the ratio.
However, ALPHAR and ALPHAI will be always less than and
usually comparable with norm(A) in magnitude, and BETA always
less than and usually comparable with norm(B).
.TP 8
VSL     (output) REAL array, dimension (LDVSL,N)
If JOBVSL = \(aqV\(aq, VSL will contain the left Schur vectors.
Not referenced if JOBVSL = \(aqN\(aq.
.TP 8
LDVSL   (input) INTEGER
The leading dimension of the matrix VSL. LDVSL >=1, and
if JOBVSL = \(aqV\(aq, LDVSL >= N.
.TP 8
VSR     (output) REAL array, dimension (LDVSR,N)
If JOBVSR = \(aqV\(aq, VSR will contain the right Schur vectors.
Not referenced if JOBVSR = \(aqN\(aq.
.TP 8
LDVSR   (input) INTEGER
The leading dimension of the matrix VSR. LDVSR >= 1, and
if JOBVSR = \(aqV\(aq, LDVSR >= N.
.TP 8
RCONDE  (output) REAL array, dimension ( 2 )
If SENSE = \(aqE\(aq or \(aqB\(aq, RCONDE(1) and RCONDE(2) contain the
reciprocal condition numbers for the average of the selected
eigenvalues.
Not referenced if SENSE = \(aqN\(aq or \(aqV\(aq.
.TP 8
RCONDV  (output) REAL array, dimension ( 2 )
If SENSE = \(aqV\(aq or \(aqB\(aq, RCONDV(1) and RCONDV(2) contain the
reciprocal condition numbers for the selected deflating
subspaces.
Not referenced if SENSE = \(aqN\(aq or \(aqE\(aq.
.TP 8
WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If N = 0, LWORK >= 1, else if SENSE = \(aqE\(aq, \(aqV\(aq, or \(aqB\(aq,
LWORK >= max( 8*N, 6*N+16, 2*SDIM*(N-SDIM) ), else
LWORK >= max( 8*N, 6*N+16 ).
Note that 2*SDIM*(N-SDIM) <= N*N/2.
Note also that an error is only returned if
LWORK < max( 8*N, 6*N+16), but if SENSE = \(aqE\(aq or \(aqV\(aq or \(aqB\(aq
this may not be large enough.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the bound on the optimal size of the WORK
array and the minimum size of the IWORK array, returns these
values as the first entries of the WORK and IWORK arrays, and
no error message related to LWORK or LIWORK is issued by
XERBLA.
.TP 8
IWORK   (workspace) INTEGER array, dimension (MAX(1,LIWORK))
On exit, if INFO = 0, IWORK(1) returns the minimum LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK.
If SENSE = \(aqN\(aq or N = 0, LIWORK >= 1, otherwise
LIWORK >= N+6.

If LIWORK = -1, then a workspace query is assumed; the
routine only calculates the bound on the optimal size of the
WORK array and the minimum size of the IWORK array, returns
these values as the first entries of the WORK and IWORK
arrays, and no error message related to LWORK or LIWORK is
issued by XERBLA.
.TP 8
BWORK   (workspace) LOGICAL array, dimension (N)
Not referenced if SORT = \(aqN\(aq.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
= 1,...,N:
The QZ iteration failed.  (A,B) are not in Schur
form, but ALPHAR(j), ALPHAI(j), and BETA(j) should
be correct for j=INFO+1,...,N.
> N:  =N+1: other than QZ iteration failed in SHGEQZ
.br
=N+2: after reordering, roundoff changed values of
some complex eigenvalues so that leading
eigenvalues in the Generalized Schur form no
longer satisfy SELCTG=.TRUE.  This could also
be caused due to scaling.
=N+3: reordering failed in STGSEN.

Further details
===============

An approximate (asymptotic) bound on the average absolute error of
the selected eigenvalues is

EPS * norm((A, B)) / RCONDE( 1 ).

An approximate (asymptotic) bound on the maximum angular error in
the computed deflating subspaces is

EPS * norm((A, B)) / RCONDV( 2 ).

See LAPACK User\(aqs Guide, section 4.11 for more information.
