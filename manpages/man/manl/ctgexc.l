.TH  CTGEXC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CTGEXC - the generalized Schur decomposition of a complex matrix pair (A,B), using an unitary equivalence transformation (A, B) := Q * (A, B) * Z\(aq, so that the diagonal block of (A, B) with row index IFST is moved to row ILST
.SH SYNOPSIS
.TP 19
SUBROUTINE CTGEXC(
WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
LDZ, IFST, ILST, INFO )
.TP 19
.ti +4
LOGICAL
WANTQ, WANTZ
.TP 19
.ti +4
INTEGER
IFST, ILST, INFO, LDA, LDB, LDQ, LDZ, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
Z( LDZ, * )
.SH PURPOSE
CTGEXC reorders the generalized Schur decomposition of a complex
matrix pair (A,B), using an unitary equivalence transformation
(A, B) := Q * (A, B) * Z\(aq, so that the diagonal block of (A, B) with
row index IFST is moved to row ILST.

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
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the upper triangular matrix A in the pair (A, B).
On exit, the updated matrix A.
.TP 8
LDA     (input)  INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,N)
On entry, the upper triangular matrix B in the pair (A, B).
On exit, the updated matrix B.
.TP 8
LDB     (input)  INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
Q       (input/output) COMPLEX array, dimension (LDZ,N)
On entry, if WANTQ = .TRUE., the unitary matrix Q.
On exit, the updated matrix Q.
If WANTQ = .FALSE., Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= 1;
If WANTQ = .TRUE., LDQ >= N.
.TP 8
Z       (input/output) COMPLEX array, dimension (LDZ,N)
On entry, if WANTZ = .TRUE., the unitary matrix Z.
On exit, the updated matrix Z.
If WANTZ = .FALSE., Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z. LDZ >= 1;
If WANTZ = .TRUE., LDZ >= N.
.TP 8
IFST    (input) INTEGER
ILST    (input/output) INTEGER
Specify the reordering of the diagonal blocks of (A, B).
The block with row index IFST is moved to row ILST, by a
sequence of swapping between adjacent blocks.
.TP 8
INFO    (output) INTEGER
=0:  Successful exit.
.br
<0:  if INFO = -i, the i-th argument had an illegal value.
.br
=1:  The transformed matrix pair (A, B) would be too far
from generalized Schur form; the problem is ill-
conditioned. (A, B) may have been partially reordered,
and ILST points to the first row of the current
position of the block being moved.
.SH FURTHER DETAILS
Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
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

