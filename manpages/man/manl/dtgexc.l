.TH  DTGEXC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DTGEXC - the generalized real Schur decomposition of a real matrix pair (A,B) using an orthogonal equivalence transformation   (A, B) = Q * (A, B) * Z\(aq,
.SH SYNOPSIS
.TP 19
SUBROUTINE DTGEXC(
WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
LDZ, IFST, ILST, WORK, LWORK, INFO )
.TP 19
.ti +4
LOGICAL
WANTQ, WANTZ
.TP 19
.ti +4
INTEGER
IFST, ILST, INFO, LDA, LDB, LDQ, LDZ, LWORK, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
WORK( * ), Z( LDZ, * )
.SH PURPOSE
DTGEXC reorders the generalized real Schur decomposition of a real
matrix pair (A,B) using an orthogonal equivalence transformation

so that the diagonal block of (A, B) with row index IFST is moved
to row ILST.
.br

(A, B) must be in generalized real Schur canonical form (as returned
by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
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
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the matrix A in generalized real Schur canonical
form.
On exit, the updated matrix A, again in generalized
real Schur canonical form.
.TP 8
LDA     (input)  INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
On entry, the matrix B in generalized real Schur canonical
form (A,B).
On exit, the updated matrix B, again in generalized
real Schur canonical form (A,B).
.TP 8
LDB     (input)  INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
Q       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
On entry, if WANTQ = .TRUE., the orthogonal matrix Q.
On exit, the updated matrix Q.
If WANTQ = .FALSE., Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= 1.
If WANTQ = .TRUE., LDQ >= N.
.TP 8
Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
On entry, if WANTZ = .TRUE., the orthogonal matrix Z.
On exit, the updated matrix Z.
If WANTZ = .FALSE., Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z. LDZ >= 1.
If WANTZ = .TRUE., LDZ >= N.
.TP 8
IFST    (input/output) INTEGER
ILST    (input/output) INTEGER
Specify the reordering of the diagonal blocks of (A, B).
The block with row index IFST is moved to row ILST, by a
sequence of swapping between adjacent blocks.
On exit, if IFST pointed on entry to the second row of
a 2-by-2 block, it is changed to point to the first row;
ILST always points to the first row of the block in its
final position (which may differ from its input value by
+1 or -1). 1 <= IFST, ILST <= N.
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
LWORK >= 1 when N <= 1, otherwise LWORK >= 4*N + 16.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
=0:  successful exit.
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

