.TH  CTGSY2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CTGSY2 - the generalized Sylvester equation   A * R - L * B = scale  D * R - L * E = scale * F  using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices,
.SH SYNOPSIS
.TP 19
SUBROUTINE CTGSY2(
TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,
LDD, E, LDE, F, LDF, SCALE, RDSUM, RDSCAL,
INFO )
.TP 19
.ti +4
CHARACTER
TRANS
.TP 19
.ti +4
INTEGER
IJOB, INFO, LDA, LDB, LDC, LDD, LDE, LDF, M, N
.TP 19
.ti +4
REAL
RDSCAL, RDSUM, SCALE
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), C( LDC, * ),
D( LDD, * ), E( LDE, * ), F( LDF, * )
.SH PURPOSE
CTGSY2 solves the generalized Sylvester equation
(A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
N-by-N and M-by-N, respectively. A, B, D and E are upper triangular
(i.e., (A,D) and (B,E) in generalized Schur form).
.br

The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
scaling factor chosen to avoid overflow.
.br

In matrix notation solving equation (1) corresponds to solve
Zx = scale * b, where Z is defined as
.br

       Z = [ kron(In, A)  -kron(B\(aq, Im) ]             (2)
           [ kron(In, D)  -kron(E\(aq, Im) ],
.br

Ik is the identity matrix of size k and X\(aq is the transpose of X.
kron(X, Y) is the Kronecker product between the matrices X and Y.

If TRANS = \(aqC\(aq, y in the conjugate transposed system Z\(aqy = scale*b
is solved for, which is equivalent to solve for R and L in

            A\(aq * R  + D\(aq * L   = scale *  C           (3)
            R  * B\(aq + L  * E\(aq  = scale * -F
.br

This case is used to compute an estimate of Dif[(A, D), (B, E)] =
= sigma_min(Z) using reverse communicaton with CLACON.
.br

CTGSY2 also (IJOB >= 1) contributes to the computation in CTGSYL
of an upper bound on the separation between to matrix pairs. Then
the input (A, D), (B, E) are sub-pencils of two matrix pairs in
CTGSYL.
.br

.SH ARGUMENTS
.TP 8
TRANS   (input) CHARACTER*1
= \(aqN\(aq, solve the generalized Sylvester equation (1).
= \(aqT\(aq: solve the \(aqtransposed\(aq system (3).
.TP 8
IJOB    (input) INTEGER
Specifies what kind of functionality to be performed.
=0: solve (1) only.
.br
=1: A contribution from this subsystem to a Frobenius
norm-based estimate of the separation between two matrix
pairs is computed. (look ahead strategy is used).
=2: A contribution from this subsystem to a Frobenius
norm-based estimate of the separation between two matrix
pairs is computed. (SGECON on sub-systems is used.)
Not referenced if TRANS = \(aqT\(aq.
.TP 8
M       (input) INTEGER
On entry, M specifies the order of A and D, and the row
dimension of C, F, R and L.
.TP 8
N       (input) INTEGER
On entry, N specifies the order of B and E, and the column
dimension of C, F, R and L.
.TP 8
A       (input) COMPLEX array, dimension (LDA, M)
On entry, A contains an upper triangular matrix.
.TP 8
LDA     (input) INTEGER
The leading dimension of the matrix A. LDA >= max(1, M).
.TP 8
B       (input) COMPLEX array, dimension (LDB, N)
On entry, B contains an upper triangular matrix.
.TP 8
LDB     (input) INTEGER
The leading dimension of the matrix B. LDB >= max(1, N).
.TP 8
C       (input/output) COMPLEX array, dimension (LDC, N)
On entry, C contains the right-hand-side of the first matrix
equation in (1).
On exit, if IJOB = 0, C has been overwritten by the solution
R.
.TP 8
LDC     (input) INTEGER
The leading dimension of the matrix C. LDC >= max(1, M).
.TP 8
D       (input) COMPLEX array, dimension (LDD, M)
On entry, D contains an upper triangular matrix.
.TP 8
LDD     (input) INTEGER
The leading dimension of the matrix D. LDD >= max(1, M).
.TP 8
E       (input) COMPLEX array, dimension (LDE, N)
On entry, E contains an upper triangular matrix.
.TP 8
LDE     (input) INTEGER
The leading dimension of the matrix E. LDE >= max(1, N).
.TP 8
F       (input/output) COMPLEX array, dimension (LDF, N)
On entry, F contains the right-hand-side of the second matrix
equation in (1).
On exit, if IJOB = 0, F has been overwritten by the solution
L.
.TP 8
LDF     (input) INTEGER
The leading dimension of the matrix F. LDF >= max(1, M).
.TP 8
SCALE   (output) REAL
On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions
R and L (C and F on entry) will hold the solutions to a
slightly perturbed system but the input matrices A, B, D and
E have not been changed. If SCALE = 0, R and L will hold the
solutions to the homogeneous system with C = F = 0.
Normally, SCALE = 1.
.TP 8
RDSUM   (input/output) REAL
On entry, the sum of squares of computed contributions to
the Dif-estimate under computation by CTGSYL, where the
scaling factor RDSCAL (see below) has been factored out.
On exit, the corresponding sum of squares updated with the
contributions from the current sub-system.
If TRANS = \(aqT\(aq RDSUM is not touched.
NOTE: RDSUM only makes sense when CTGSY2 is called by
CTGSYL.
.TP 8
RDSCAL  (input/output) REAL
On entry, scaling factor used to prevent overflow in RDSUM.
On exit, RDSCAL is updated w.r.t. the current contributions
in RDSUM.
If TRANS = \(aqT\(aq, RDSCAL is not touched.
NOTE: RDSCAL only makes sense when CTGSY2 is called by
CTGSYL.
.TP 8
INFO    (output) INTEGER
On exit, if INFO is set to
=0: Successful exit
.br
<0: If INFO = -i, input argument number i is illegal.
.br
>0: The matrix pairs (A, D) and (B, E) have common or very
close eigenvalues.
.SH FURTHER DETAILS
Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
.br

