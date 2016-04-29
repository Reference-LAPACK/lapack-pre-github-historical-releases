.TH  STGEVC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
STGEVC - some or all of the right and/or left eigenvectors of a pair of real matrices (S,P), where S is a quasi-triangular matrix and P is upper triangular
.SH SYNOPSIS
.TP 19
SUBROUTINE STGEVC(
SIDE, HOWMNY, SELECT, N, S, LDS, P, LDP, VL,
LDVL, VR, LDVR, MM, M, WORK, INFO )
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
REAL
P( LDP, * ), S( LDS, * ), VL( LDVL, * ),
VR( LDVR, * ), WORK( * )
.SH PURPOSE
STGEVC computes some or all of the right and/or left eigenvectors of
a pair of real matrices (S,P), where S is a quasi-triangular matrix
and P is upper triangular.  Matrix pairs of this type are produced by
the generalized Schur factorization of a matrix pair (A,B):

   A = Q*S*Z**T,  B = Q*P*Z**T
.br

as computed by SGGHRD + SHGEQZ.
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
directly from the diagonal blocks of S and P.
.br

.br
This routine returns the matrices X and/or Y of right and left
eigenvectors of (S,P), or the products Z*X and/or Q*Y,
.br
where Z and Q are input matrices.
.br
If Q and Z are the orthogonal factors from the generalized Schur
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
computed.  If w(j) is a real eigenvalue, the corresponding
real eigenvector is computed if SELECT(j) is .TRUE..
If w(j) and w(j+1) are the real and imaginary parts of a
complex eigenvalue, the corresponding complex eigenvector
is computed if either SELECT(j) or SELECT(j+1) is .TRUE.,
and on exit SELECT(j) is set to .TRUE. and SELECT(j+1) is
set to .FALSE..
Not referenced if HOWMNY = \(aqA\(aq or \(aqB\(aq.
.TP 8
N       (input) INTEGER
The order of the matrices S and P.  N >= 0.
.TP 8
S       (input) REAL array, dimension (LDS,N)
The upper quasi-triangular matrix S from a generalized Schur
factorization, as computed by SHGEQZ.
.TP 8
LDS     (input) INTEGER
The leading dimension of array S.  LDS >= max(1,N).
.TP 8
P       (input) REAL array, dimension (LDP,N)
The upper triangular matrix P from a generalized Schur
factorization, as computed by SHGEQZ.
2-by-2 diagonal blocks of P corresponding to 2-by-2 blocks
of S must be in positive diagonal form.
.TP 8
LDP     (input) INTEGER
The leading dimension of array P.  LDP >= max(1,N).
.TP 8
VL      (input/output) REAL array, dimension (LDVL,MM)
On entry, if SIDE = \(aqL\(aq or \(aqB\(aq and HOWMNY = \(aqB\(aq, VL must
contain an N-by-N matrix Q (usually the orthogonal matrix Q
of left Schur vectors returned by SHGEQZ).
On exit, if SIDE = \(aqL\(aq or \(aqB\(aq, VL contains:
if HOWMNY = \(aqA\(aq, the matrix Y of left eigenvectors of (S,P);
if HOWMNY = \(aqB\(aq, the matrix Q*Y;
if HOWMNY = \(aqS\(aq, the left eigenvectors of (S,P) specified by
SELECT, stored consecutively in the columns of
VL, in the same order as their eigenvalues.

A complex eigenvector corresponding to a complex eigenvalue
is stored in two consecutive columns, the first holding the
real part, and the second the imaginary part.

Not referenced if SIDE = \(aqR\(aq.
.TP 8
LDVL    (input) INTEGER
The leading dimension of array VL.  LDVL >= 1, and if
SIDE = \(aqL\(aq or \(aqB\(aq, LDVL >= N.
.TP 8
VR      (input/output) REAL array, dimension (LDVR,MM)
On entry, if SIDE = \(aqR\(aq or \(aqB\(aq and HOWMNY = \(aqB\(aq, VR must
contain an N-by-N matrix Z (usually the orthogonal matrix Z
of right Schur vectors returned by SHGEQZ).

On exit, if SIDE = \(aqR\(aq or \(aqB\(aq, VR contains:
if HOWMNY = \(aqA\(aq, the matrix X of right eigenvectors of (S,P);
if HOWMNY = \(aqB\(aq or \(aqb\(aq, the matrix Z*X;
if HOWMNY = \(aqS\(aq or \(aqs\(aq, the right eigenvectors of (S,P)
specified by SELECT, stored consecutively in the
columns of VR, in the same order as their
eigenvalues.

A complex eigenvector corresponding to a complex eigenvalue
is stored in two consecutive columns, the first holding the
real part and the second the imaginary part.
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
is set to N.  Each selected real eigenvector occupies one
column and each selected complex eigenvector occupies two
columns.
.TP 8
WORK    (workspace) REAL array, dimension (6*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  the 2-by-2 block (INFO:INFO+1) does not have a complex
eigenvalue.
.SH FURTHER DETAILS
Allocation of workspace:
.br
---------- -- ---------
.br

   WORK( j ) = 1-norm of j-th column of A, above the diagonal
   WORK( N+j ) = 1-norm of j-th column of B, above the diagonal
   WORK( 2*N+1:3*N ) = real part of eigenvector
.br
   WORK( 3*N+1:4*N ) = imaginary part of eigenvector
.br
   WORK( 4*N+1:5*N ) = real part of back-transformed eigenvector
   WORK( 5*N+1:6*N ) = imaginary part of back-transformed eigenvector

Rowwise vs. columnwise solution methods:
.br
------- --  ---------- -------- -------
.br

Finding a generalized eigenvector consists basically of solving the
singular triangular system
.br

 (A - w B) x = 0     (for right) or:   (A - w B)**H y = 0  (for left)

Consider finding the i-th right eigenvector (assume all eigenvalues
are real). The equation to be solved is:
.br
     n                   i
.br
0 = sum  C(j,k) v(k)  = sum  C(j,k) v(k)     for j = i,. . .,1
    k=j                 k=j
.br

where  C = (A - w B)  (The components v(i+1:n) are 0.)
.br

The "rowwise" method is:
.br

(1)  v(i) := 1
.br
for j = i-1,. . .,1:
.br
                        i
.br
    (2) compute  s = - sum C(j,k) v(k)   and
.br
                      k=j+1
.br

    (3) v(j) := s / C(j,j)
.br

Step 2 is sometimes called the "dot product" step, since it is an
inner product between the j-th row and the portion of the eigenvector
that has been computed so far.
.br

The "columnwise" method consists basically in doing the sums
for all the rows in parallel.  As each v(j) is computed, the
contribution of v(j) times the j-th column of C is added to the
partial sums.  Since FORTRAN arrays are stored columnwise, this has
the advantage that at each step, the elements of C that are accessed
are adjacent to one another, whereas with the rowwise method, the
elements accessed at a step are spaced LDS (and LDP) words apart.

When finding left eigenvectors, the matrix in question is the
transpose of the one in storage, so the rowwise method then
actually accesses columns of A and B at each step, and so is the
preferred method.
.br

