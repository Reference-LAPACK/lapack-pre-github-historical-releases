.TH  ZTGEX2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZTGEX2 - adjacent diagonal 1 by 1 blocks (A11,B11) and (A22,B22)
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTGEX2(
WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
LDZ, J1, INFO )
.TP 19
.ti +4
LOGICAL
WANTQ, WANTZ
.TP 19
.ti +4
INTEGER
INFO, J1, LDA, LDB, LDQ, LDZ, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
Z( LDZ, * )
.SH PURPOSE
ZTGEX2 swaps adjacent diagonal 1 by 1 blocks (A11,B11) and (A22,B22)
in an upper triangular matrix pair (A, B) by an unitary equivalence
transformation.
.br

(A, B) must be in generalized Schur canonical form, that is, A and
B are both upper triangular.
.br

Optionally, the matrices Q and Z of generalized Schur vectors are
updated.
.br

       Q(in) * A(in) * Z(in)\(aq = Q(out) * A(out) * Z(out)\(aq
       Q(in) * B(in) * Z(in)\(aq = Q(out) * B(out) * Z(out)\(aq


.SH ARGUMENTS
.TP 8
WANTQ   (input) LOGICAL
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
N       (input) INTEGER
The order of the matrices A and B. N >= 0.
.TP 8
A       (input/output) COMPLEX*16 arrays, dimensions (LDA,N)
On entry, the matrix A in the pair (A, B).
On exit, the updated matrix A.
.TP 8
LDA     (input)  INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX*16 arrays, dimensions (LDB,N)
On entry, the matrix B in the pair (A, B).
On exit, the updated matrix B.
.TP 8
LDB     (input)  INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
Q       (input/output) COMPLEX*16 array, dimension (LDZ,N)
If WANTQ = .TRUE, on entry, the unitary matrix Q. On exit,
the updated matrix Q.
Not referenced if WANTQ = .FALSE..
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= 1;
If WANTQ = .TRUE., LDQ >= N.
.TP 8
Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)
If WANTZ = .TRUE, on entry, the unitary matrix Z. On exit,
the updated matrix Z.
Not referenced if WANTZ = .FALSE..
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z. LDZ >= 1;
If WANTZ = .TRUE., LDZ >= N.
.TP 8
J1      (input) INTEGER
The index to the first block (A11, B11).
.TP 8
INFO    (output) INTEGER
=0:  Successful exit.
.br
=1:  The transformed matrix pair (A, B) would be too far
from generalized Schur form; the problem is ill-
conditioned. 
.SH FURTHER DETAILS
Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
.br

In the current code both weak and strong stability tests are
performed. The user can omit the strong stability test by changing
the internal logical parameter WANDS to .FALSE.. See ref. [2] for
details.
.br

[1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
    Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
    M.S. Moonen et al (eds), Linear Algebra for Large Scale and
    Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.

[2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
    Eigenvalues of a Regular Matrix Pair (A, B) and Condition
    Estimation: Theory, Algorithms and Software, Report UMINF-94.04,
    Department of Computing Science, Umea University, S-901 87 Umea,
    Sweden, 1994. Also as LAPACK Working Note 87. To appear in
    Numerical Algorithms, 1996.
.br

