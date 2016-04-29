.TH  CTGSEN 1 "February 2007" " LAPACK routine (version 3.1.1) " " LAPACK routine (version 3.1.1) " 
.SH NAME
CTGSEN - the generalized Schur decomposition of a complex matrix pair (A, B) (in terms of an unitary equivalence trans- formation Q\(aq * (A, B) * Z), so that a selected cluster of eigenvalues appears in the leading diagonal blocks of the pair (A,B)
.SH SYNOPSIS
.TP 19
SUBROUTINE CTGSEN(
IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB,
ALPHA, BETA, Q, LDQ, Z, LDZ, M, PL, PR, DIF,
WORK, LWORK, IWORK, LIWORK, INFO )
.TP 19
.ti +4
LOGICAL
WANTQ, WANTZ
.TP 19
.ti +4
INTEGER
IJOB, INFO, LDA, LDB, LDQ, LDZ, LIWORK, LWORK,
M, N
.TP 19
.ti +4
REAL
PL, PR
.TP 19
.ti +4
LOGICAL
SELECT( * )
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
DIF( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), ALPHA( * ), B( LDB, * ),
BETA( * ), Q( LDQ, * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
CTGSEN reorders the generalized Schur decomposition of a complex
matrix pair (A, B) (in terms of an unitary equivalence trans-
formation Q\(aq * (A, B) * Z), so that a selected cluster of eigenvalues
appears in the leading diagonal blocks of the pair (A,B). The leading
columns of Q and Z form unitary bases of the corresponding left and
right eigenspaces (deflating subspaces). (A, B) must be in
generalized Schur canonical form, that is, A and B are both upper
triangular.
.br

CTGSEN also computes the generalized eigenvalues
.br

         w(j)= ALPHA(j) / BETA(j)
.br

of the reordered matrix pair (A, B).
.br

Optionally, the routine computes estimates of reciprocal condition
numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
(A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
between the matrix pairs (A11, B11) and (A22,B22) that correspond to
the selected cluster and the eigenvalues outside the cluster, resp.,
and norms of "projections" onto left and right eigenspaces w.r.t.
the selected cluster in the (1,1)-block.
.br


.SH ARGUMENTS
.TP 8
IJOB    (input) integer
Specifies whether condition numbers are required for the
cluster of eigenvalues (PL and PR) or the deflating subspaces
(Difu and Difl):
.br
=0: Only reorder w.r.t. SELECT. No extras.
.br
=1: Reciprocal of norms of "projections" onto left and right
eigenspaces w.r.t. the selected cluster (PL and PR).
=2: Upper bounds on Difu and Difl. F-norm-based estimate
.br
(DIF(1:2)).
.br
=3: Estimate of Difu and Difl. 1-norm-based estimate
.br
(DIF(1:2)).
About 5 times as expensive as IJOB = 2.
=4: Compute PL, PR and DIF (i.e. 0, 1 and 2 above): Economic
version to get it all.
=5: Compute PL, PR and DIF (i.e. 0, 1 and 3 above)
.TP 8
WANTQ   (input) LOGICAL
.br
.TRUE. : update the left transformation matrix Q;
.br
.FALSE.: do not update Q.
.TP 8
WANTZ   (input) LOGICAL
.br
.TRUE. : update the right transformation matrix Z;
.br
.FALSE.: do not update Z.
.TP 8
SELECT  (input) LOGICAL array, dimension (N)
SELECT specifies the eigenvalues in the selected cluster. To
select an eigenvalue w(j), SELECT(j) must be set to
.TRUE..
.TP 8
N       (input) INTEGER
The order of the matrices A and B. N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension(LDA,N)
On entry, the upper triangular matrix A, in generalized
Schur canonical form.
On exit, A is overwritten by the reordered matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX array, dimension(LDB,N)
On entry, the upper triangular matrix B, in generalized
Schur canonical form.
On exit, B is overwritten by the reordered matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
ALPHA   (output) COMPLEX array, dimension (N)
BETA    (output) COMPLEX array, dimension (N)
The diagonal elements of A and B, respectively,
when the pair (A,B) has been reduced to generalized Schur
form.  ALPHA(i)/BETA(i) i=1,...,N are the generalized
eigenvalues.
.TP 8
Q       (input/output) COMPLEX array, dimension (LDQ,N)
On entry, if WANTQ = .TRUE., Q is an N-by-N matrix.
On exit, Q has been postmultiplied by the left unitary
transformation matrix which reorder (A, B); The leading M
columns of Q form orthonormal bases for the specified pair of
left eigenspaces (deflating subspaces).
If WANTQ = .FALSE., Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= 1.
If WANTQ = .TRUE., LDQ >= N.
.TP 8
Z       (input/output) COMPLEX array, dimension (LDZ,N)
On entry, if WANTZ = .TRUE., Z is an N-by-N matrix.
On exit, Z has been postmultiplied by the left unitary
transformation matrix which reorder (A, B); The leading M
columns of Z form orthonormal bases for the specified pair of
left eigenspaces (deflating subspaces).
If WANTZ = .FALSE., Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z. LDZ >= 1.
If WANTZ = .TRUE., LDZ >= N.
.TP 8
M       (output) INTEGER
The dimension of the specified pair of left and right
eigenspaces, (deflating subspaces) 0 <= M <= N.
.TP 6
PL	   (output) REAL
PR	   (output) REAL
If IJOB = 1, 4 or 5, PL, PR are lower bounds on the
reciprocal  of the norm of "projections" onto left and right
eigenspace with respect to the selected cluster.
0 < PL, PR <= 1.
If M = 0 or M = N, PL = PR  = 1.
If IJOB = 0, 2 or 3 PL, PR are not referenced.
.TP 8
DIF     (output) REAL array, dimension (2).
If IJOB >= 2, DIF(1:2) store the estimates of Difu and Difl.
.br
If IJOB = 2 or 4, DIF(1:2) are F-norm-based upper bounds on
.br
Difu and Difl. If IJOB = 3 or 5, DIF(1:2) are 1-norm-based
estimates of Difu and Difl, computed using reversed
communication with CLACN2.
If M = 0 or N, DIF(1:2) = F-norm([A, B]).
If IJOB = 0 or 1, DIF is not referenced.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
IF IJOB = 0, WORK is not referenced.  Otherwise,
on exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >=  1
If IJOB = 1, 2 or 4, LWORK >=  2*M*(N-M)
If IJOB = 3 or 5, LWORK >=  4*M*(N-M)

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
IF IJOB = 0, IWORK is not referenced.  Otherwise,
on exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK. LIWORK >= 1.
If IJOB = 1, 2 or 4, LIWORK >=  N+2;
If IJOB = 3 or 5, LIWORK >= MAX(N+2, 2*M*(N-M));

If LIWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal size of the IWORK array,
returns this value as the first entry of the IWORK array, and
no error message related to LIWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
=0: Successful exit.
.br
<0: If INFO = -i, the i-th argument had an illegal value.
.br
=1: Reordering of (A, B) failed because the transformed
matrix pair (A, B) would be too far from generalized
Schur form; the problem is very ill-conditioned.
(A, B) may have been partially reordered.
If requested, 0 is returned in DIF(*), PL and PR.
.SH FURTHER DETAILS
CTGSEN first collects the selected eigenvalues by computing unitary
U and W that move them to the top left corner of (A, B). In other
words, the selected eigenvalues are the eigenvalues of (A11, B11) in

              U\(aq*(A, B)*W = (A11 A12) (B11 B12) n1
.br
                            ( 0  A22),( 0  B22) n2
.br
                              n1  n2    n1  n2
.br

where N = n1+n2 and U\(aq means the conjugate transpose of U. The first
n1 columns of U and W span the specified pair of left and right
eigenspaces (deflating subspaces) of (A, B).
.br

If (A, B) has been obtained from the generalized real Schur
decomposition of a matrix pair (C, D) = Q*(A, B)*Z\(aq, then the
reordered generalized Schur form of (C, D) is given by
.br

         (C, D) = (Q*U)*(U\(aq*(A, B)*W)*(Z*W)\(aq,
.br

and the first n1 columns of Q*U and Z*W span the corresponding
deflating subspaces of (C, D) (Q and Z store Q*U and Z*W, resp.).

Note that if the selected eigenvalue is sufficiently ill-conditioned,
then its value may differ significantly from its value before
reordering.
.br

The reciprocal condition numbers of the left and right eigenspaces
spanned by the first n1 columns of U and W (or Q*U and Z*W) may
be returned in DIF(1:2), corresponding to Difu and Difl, resp.

The Difu and Difl are defined as:
.br

     Difu[(A11, B11), (A22, B22)] = sigma-min( Zu )
.br
and
.br
     Difl[(A11, B11), (A22, B22)] = Difu[(A22, B22), (A11, B11)],

where sigma-min(Zu) is the smallest singular value of the
(2*n1*n2)-by-(2*n1*n2) matrix
.br

     Zu = [ kron(In2, A11)  -kron(A22\(aq, In1) ]
.br
          [ kron(In2, B11)  -kron(B22\(aq, In1) ].
.br

Here, Inx is the identity matrix of size nx and A22\(aq is the
transpose of A22. kron(X, Y) is the Kronecker product between
the matrices X and Y.
.br

When DIF(2) is small, small changes in (A, B) can cause large changes
in the deflating subspace. An approximate (asymptotic) bound on the
maximum angular error in the computed deflating subspaces is

     EPS * norm((A, B)) / DIF(2),
.br

where EPS is the machine precision.
.br

The reciprocal norm of the projectors on the left and right
eigenspaces associated with (A11, B11) may be returned in PL and PR.
They are computed as follows. First we compute L and R so that
P*(A, B)*Q is block diagonal, where
.br

     P = ( I -L ) n1           Q = ( I R ) n1
.br
         ( 0  I ) n2    and        ( 0 I ) n2
.br
           n1 n2                    n1 n2
.br

and (L, R) is the solution to the generalized Sylvester equation

     A11*R - L*A22 = -A12
.br
     B11*R - L*B22 = -B12
.br

Then PL = (F-norm(L)**2+1)**(-1/2) and PR = (F-norm(R)**2+1)**(-1/2).
An approximate (asymptotic) bound on the average absolute error of
the selected eigenvalues is
.br

     EPS * norm((A, B)) / PL.
.br

There are also global error bounds which valid for perturbations up
to a certain restriction:  A lower bound (x) on the smallest
F-norm(E,F) for which an eigenvalue of (A11, B11) may move and
coalesce with an eigenvalue of (A22, B22) under perturbation (E,F),
(i.e. (A + E, B + F), is
.br

 x = min(Difu,Difl)/((1/(PL*PL)+1/(PR*PR))**(1/2)+2*max(1/PL,1/PR)).

An approximate bound on x can be computed from DIF(1:2), PL and PR.

If y = ( F-norm(E,F) / x) <= 1, the angles between the perturbed
(L\(aq, R\(aq) and unperturbed (L, R) left and right deflating subspaces
associated with the selected cluster in the (1,1)-blocks can be
bounded as
.br

 max-angle(L, L\(aq) <= arctan( y * PL / (1 - y * (1 - PL * PL)**(1/2))
 max-angle(R, R\(aq) <= arctan( y * PR / (1 - y * (1 - PR * PR)**(1/2))

See LAPACK User\(aqs Guide section 4.11 or the following references
for more information.
.br

Note that if the default method for computing the Frobenius-norm-
based estimate DIF is not wanted (see CLATDF), then the parameter
IDIFJB (see below) should be changed from 3 to 4 (routine CLATDF
(IJOB = 2 will be used)). See CTGSYL for more details.
.br

Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
.br

References
.br
==========
.br

[1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
    Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
    M.S. Moonen et al (eds), Linear Algebra for Large Scale and
    Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.

[2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
    Eigenvalues of a Regular Matrix Pair (A, B) and Condition
    Estimation: Theory, Algorithms and Software, Report
.br
    UMINF - 94.04, Department of Computing Science, Umea University,
    S-901 87 Umea, Sweden, 1994. Also as LAPACK Working Note 87.
    To appear in Numerical Algorithms, 1996.
.br

[3] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
    for Solving the Generalized Sylvester Equation and Estimating the
    Separation between Regular Matrix Pairs, Report UMINF - 93.23,
    Department of Computing Science, Umea University, S-901 87 Umea,
    Sweden, December 1993, Revised April 1994, Also as LAPACK working
    Note 75. To appear in ACM Trans. on Math. Software, Vol 22, No 1,
    1996.
.br

