.TH  STGEX2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
STGEX2 - adjacent diagonal blocks (A11, B11) and (A22, B22) of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair (A, B) by an orthogonal equivalence transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE STGEX2(
WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
LDZ, J1, N1, N2, WORK, LWORK, INFO )
.TP 19
.ti +4
LOGICAL
WANTQ, WANTZ
.TP 19
.ti +4
INTEGER
INFO, J1, LDA, LDB, LDQ, LDZ, LWORK, N, N1, N2
.TP 19
.ti +4
REAL
A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
WORK( * ), Z( LDZ, * )
.SH PURPOSE
STGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22)
of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair
(A, B) by an orthogonal equivalence transformation.

(A, B) must be in generalized real Schur canonical form (as returned
by SGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
diagonal blocks. B is upper triangular.
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
.TP 7
A      (input/output) REAL arrays, dimensions (LDA,N)
On entry, the matrix A in the pair (A, B).
On exit, the updated matrix A.
.TP 8
LDA     (input)  INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 7
B      (input/output) REAL arrays, dimensions (LDB,N)
On entry, the matrix B in the pair (A, B).
On exit, the updated matrix B.
.TP 8
LDB     (input)  INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
Q       (input/output) REAL array, dimension (LDZ,N)
On entry, if WANTQ = .TRUE., the orthogonal matrix Q.
On exit, the updated matrix Q.
Not referenced if WANTQ = .FALSE..
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= 1.
If WANTQ = .TRUE., LDQ >= N.
.TP 8
Z       (input/output) REAL array, dimension (LDZ,N)
On entry, if WANTZ =.TRUE., the orthogonal matrix Z.
On exit, the updated matrix Z.
Not referenced if WANTZ = .FALSE..
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z. LDZ >= 1.
If WANTZ = .TRUE., LDZ >= N.
.TP 8
J1      (input) INTEGER
The index to the first block (A11, B11). 1 <= J1 <= N.
.TP 8
N1      (input) INTEGER
The order of the first block (A11, B11). N1 = 0, 1 or 2.
.TP 8
N2      (input) INTEGER
The order of the second block (A22, B22). N2 = 0, 1 or 2.
.TP 8
WORK    (workspace) REAL array, dimension (MAX(1,LWORK)).
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
LWORK >=  MAX( N*(N2+N1), (N2+N1)*(N2+N1)*2 )
.TP 8
INFO    (output) INTEGER
=0: Successful exit
.br
>0: If INFO = 1, the transformed matrix (A, B) would be
too far from generalized Schur form; the blocks are
not swapped and (A, B) and (Q, Z) are unchanged.
The problem of swapping is too ill-conditioned.
<0: If INFO = -16: LWORK is too small. Appropriate value
for LWORK is returned in WORK(1).
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
    Estimation: Theory, Algorithms and Software,
.br
    Report UMINF - 94.04, Department of Computing Science, Umea
    University, S-901 87 Umea, Sweden, 1994. Also as LAPACK Working
    Note 87. To appear in Numerical Algorithms, 1996.
.br

