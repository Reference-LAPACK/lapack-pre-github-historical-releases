.TH  ZTREVC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTREVC - some or all of the right and/or left eigenvectors of a complex upper triangular matrix T
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTREVC(
SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
LDVR, MM, M, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
HOWMNY, SIDE
.TP 19
.ti +4
INTEGER
INFO, LDT, LDVL, LDVR, M, MM, N
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
T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
WORK( * )
.SH PURPOSE
ZTREVC computes some or all of the right and/or left eigenvectors of
a complex upper triangular matrix T.
Matrices of this type are produced by the Schur factorization of
a complex general matrix:  A = Q*T*Q**H, as computed by ZHSEQR.

.br
The right eigenvector x and the left eigenvector y of T corresponding
to an eigenvalue w are defined by:
.br

.br
             T*x = w*x,     (y**H)*T = w*(y**H)
.br

.br
where y**H denotes the conjugate transpose of the vector y.
The eigenvalues are not input to this routine, but are read directly
from the diagonal of T.
.br

.br
This routine returns the matrices X and/or Y of right and left
eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
input matrix.  If Q is the unitary factor that reduces a matrix A to
Schur form T, then Q*X and Q*Y are the matrices of right and left
eigenvectors of A.
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= \(aqR\(aq:  compute right eigenvectors only;
.br
= \(aqL\(aq:  compute left eigenvectors only;
.br
= \(aqB\(aq:  compute both right and left eigenvectors.
.TP 8
HOWMNY  (input) CHARACTER*1
.br
= \(aqA\(aq:  compute all right and/or left eigenvectors;
.br
= \(aqB\(aq:  compute all right and/or left eigenvectors,
backtransformed using the matrices supplied in
VR and/or VL;
= \(aqS\(aq:  compute selected right and/or left eigenvectors,
as indicated by the logical array SELECT.
.TP 8
SELECT  (input) LOGICAL array, dimension (N)
If HOWMNY = \(aqS\(aq, SELECT specifies the eigenvectors to be
computed.
The eigenvector corresponding to the j-th eigenvalue is
computed if SELECT(j) = .TRUE..
Not referenced if HOWMNY = \(aqA\(aq or \(aqB\(aq.
.TP 8
N       (input) INTEGER
The order of the matrix T. N >= 0.
.TP 8
T       (input/output) COMPLEX*16 array, dimension (LDT,N)
The upper triangular matrix T.  T is modified, but restored
on exit.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= max(1,N).
.TP 8
VL      (input/output) COMPLEX*16 array, dimension (LDVL,MM)
On entry, if SIDE = \(aqL\(aq or \(aqB\(aq and HOWMNY = \(aqB\(aq, VL must
contain an N-by-N matrix Q (usually the unitary matrix Q of
Schur vectors returned by ZHSEQR).
On exit, if SIDE = \(aqL\(aq or \(aqB\(aq, VL contains:
if HOWMNY = \(aqA\(aq, the matrix Y of left eigenvectors of T;
if HOWMNY = \(aqB\(aq, the matrix Q*Y;
if HOWMNY = \(aqS\(aq, the left eigenvectors of T specified by
SELECT, stored consecutively in the columns
of VL, in the same order as their
eigenvalues.
Not referenced if SIDE = \(aqR\(aq.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL.  LDVL >= 1, and if
SIDE = \(aqL\(aq or \(aqB\(aq, LDVL >= N.
.TP 8
VR      (input/output) COMPLEX*16 array, dimension (LDVR,MM)
On entry, if SIDE = \(aqR\(aq or \(aqB\(aq and HOWMNY = \(aqB\(aq, VR must
contain an N-by-N matrix Q (usually the unitary matrix Q of
Schur vectors returned by ZHSEQR).
On exit, if SIDE = \(aqR\(aq or \(aqB\(aq, VR contains:
if HOWMNY = \(aqA\(aq, the matrix X of right eigenvectors of T;
if HOWMNY = \(aqB\(aq, the matrix Q*X;
if HOWMNY = \(aqS\(aq, the right eigenvectors of T specified by
SELECT, stored consecutively in the columns
of VR, in the same order as their
eigenvalues.
Not referenced if SIDE = \(aqL\(aq.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.  LDVR >= 1, and if
SIDE = \(aqR\(aq or \(aqB\(aq; LDVR >= N.
.TP 8
MM      (input) INTEGER
The number of columns in the arrays VL and/or VR. MM >= M.
.TP 8
M       (output) INTEGER
The number of columns in the arrays VL and/or VR actually
used to store the eigenvectors.  If HOWMNY = \(aqA\(aq or \(aqB\(aq, M
is set to N.  Each selected eigenvector occupies one
column.
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (2*N)
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The algorithm used in this program is basically backward (forward)
substitution, with scaling to make the the code robust against
possible overflow.
.br

Each eigenvector is normalized so that the element of largest
magnitude has magnitude 1; here the magnitude of a complex number
(x,y) is taken to be |x| + |y|.
.br

