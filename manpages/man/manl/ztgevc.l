.TH  ZTGEVC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTGEVC - some or all of the right and/or left eigenvectors of a pair of complex matrices (S,P), where S and P are upper triangular
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTGEVC(
SIDE, HOWMNY, SELECT, N, S, LDS, P, LDP, VL,
LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
HOWMNY, SIDE
.TP 19
.ti +4
INTEGER
INFO, LDP, LDS, LDVL, LDVR, M, MM, N
.TP 19
.ti +4
LOGICAL
SELECT( * )
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * )
.TP 19
.ti +4
COMPLEX*16
P( LDP, * ), S( LDS, * ), VL( LDVL, * ),
VR( LDVR, * ), WORK( * )
.SH PURPOSE
ZTGEVC computes some or all of the right and/or left eigenvectors of
a pair of complex matrices (S,P), where S and P are upper triangular.
Matrix pairs of this type are produced by the generalized Schur
factorization of a complex matrix pair (A,B):
.br

.br
   A = Q*S*Z**H,  B = Q*P*Z**H
.br

.br
as computed by ZGGHRD + ZHGEQZ.
.br

.br
The right eigenvector x and the left eigenvector y of (S,P)
corresponding to an eigenvalue w are defined by:
.br

.br
   S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
.br

.br
where y**H denotes the conjugate tranpose of y.
.br
The eigenvalues are not input to this routine, but are computed
directly from the diagonal elements of S and P.
.br

.br
This routine returns the matrices X and/or Y of right and left
eigenvectors of (S,P), or the products Z*X and/or Q*Y,
.br
where Z and Q are input matrices.
.br
If Q and Z are the unitary factors from the generalized Schur
factorization of a matrix pair (A,B), then Z*X and Q*Y
.br
are the matrices of right and left eigenvectors of (A,B).

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= \(aqR\(aq: compute right eigenvectors only;
.br
= \(aqL\(aq: compute left eigenvectors only;
.br
= \(aqB\(aq: compute both right and left eigenvectors.
.TP 8
HOWMNY  (input) CHARACTER*1
.br
= \(aqA\(aq: compute all right and/or left eigenvectors;
.br
= \(aqB\(aq: compute all right and/or left eigenvectors,
backtransformed by the matrices in VR and/or VL;
= \(aqS\(aq: compute selected right and/or left eigenvectors,
specified by the logical array SELECT.
.TP 8
SELECT  (input) LOGICAL array, dimension (N)
If HOWMNY=\(aqS\(aq, SELECT specifies the eigenvectors to be
computed.  The eigenvector corresponding to the j-th
eigenvalue is computed if SELECT(j) = .TRUE..
Not referenced if HOWMNY = \(aqA\(aq or \(aqB\(aq.
.TP 8
N       (input) INTEGER
The order of the matrices S and P.  N >= 0.
.TP 8
S       (input) COMPLEX*16 array, dimension (LDS,N)
The upper triangular matrix S from a generalized Schur
factorization, as computed by ZHGEQZ.
.TP 8
LDS     (input) INTEGER
The leading dimension of array S.  LDS >= max(1,N).
.TP 8
P       (input) COMPLEX*16 array, dimension (LDP,N)
The upper triangular matrix P from a generalized Schur
factorization, as computed by ZHGEQZ.  P must have real
diagonal elements.
.TP 8
LDP     (input) INTEGER
The leading dimension of array P.  LDP >= max(1,N).
.TP 8
VL      (input/output) COMPLEX*16 array, dimension (LDVL,MM)
On entry, if SIDE = \(aqL\(aq or \(aqB\(aq and HOWMNY = \(aqB\(aq, VL must
contain an N-by-N matrix Q (usually the unitary matrix Q
of left Schur vectors returned by ZHGEQZ).
On exit, if SIDE = \(aqL\(aq or \(aqB\(aq, VL contains:
if HOWMNY = \(aqA\(aq, the matrix Y of left eigenvectors of (S,P);
if HOWMNY = \(aqB\(aq, the matrix Q*Y;
if HOWMNY = \(aqS\(aq, the left eigenvectors of (S,P) specified by
SELECT, stored consecutively in the columns of
VL, in the same order as their eigenvalues.
Not referenced if SIDE = \(aqR\(aq.
.TP 8
LDVL    (input) INTEGER
The leading dimension of array VL.  LDVL >= 1, and if
SIDE = \(aqL\(aq or \(aql\(aq or \(aqB\(aq or \(aqb\(aq, LDVL >= N.
.TP 8
VR      (input/output) COMPLEX*16 array, dimension (LDVR,MM)
On entry, if SIDE = \(aqR\(aq or \(aqB\(aq and HOWMNY = \(aqB\(aq, VR must
contain an N-by-N matrix Q (usually the unitary matrix Z
of right Schur vectors returned by ZHGEQZ).
On exit, if SIDE = \(aqR\(aq or \(aqB\(aq, VR contains:
if HOWMNY = \(aqA\(aq, the matrix X of right eigenvectors of (S,P);
if HOWMNY = \(aqB\(aq, the matrix Z*X;
if HOWMNY = \(aqS\(aq, the right eigenvectors of (S,P) specified by
SELECT, stored consecutively in the columns of
VR, in the same order as their eigenvalues.
Not referenced if SIDE = \(aqL\(aq.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.  LDVR >= 1, and if
SIDE = \(aqR\(aq or \(aqB\(aq, LDVR >= N.
.TP 8
MM      (input) INTEGER
The number of columns in the arrays VL and/or VR. MM >= M.
.TP 8
M       (output) INTEGER
The number of columns in the arrays VL and/or VR actually
used to store the eigenvectors.  If HOWMNY = \(aqA\(aq or \(aqB\(aq, M
is set to N.  Each selected eigenvector occupies one column.
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (2*N)
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
