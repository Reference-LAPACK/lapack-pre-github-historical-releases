.TH  DTGSYL 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DTGSYL - the generalized Sylvester equation
.SH SYNOPSIS
.TP 19
SUBROUTINE DTGSYL(
TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,
LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,
IWORK, INFO )
.TP 19
.ti +4
CHARACTER
TRANS
.TP 19
.ti +4
INTEGER
IJOB, INFO, LDA, LDB, LDC, LDD, LDE, LDF,
LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION DIF, SCALE
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), C( LDC, * ),
D( LDD, * ), E( LDE, * ), F( LDF, * ),
WORK( * )
.SH PURPOSE
DTGSYL solves the generalized Sylvester equation:

            A * R - L * B = scale * C                 (1)
            D * R - L * E = scale * F
.br

where R and L are unknown m-by-n matrices, (A, D), (B, E) and
(C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
respectively, with real entries. (A, D) and (B, E) must be in
generalized (real) Schur canonical form, i.e. A, B are upper quasi
triangular and D, E are upper triangular.
.br

The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
scaling factor chosen to avoid overflow.
.br

In matrix notation (1) is equivalent to solve  Zx = scale b, where
Z is defined as
.br

           Z = [ kron(In, A)  -kron(B\(aq, Im) ]         (2)
               [ kron(In, D)  -kron(E\(aq, Im) ].
.br

Here Ik is the identity matrix of size k and X\(aq is the transpose of
X. kron(X, Y) is the Kronecker product between the matrices X and Y.

If TRANS = \(aqT\(aq, DTGSYL solves the transposed system Z\(aq*y = scale*b,
which is equivalent to solve for R and L in
.br

            A\(aq * R  + D\(aq * L   = scale *  C           (3)
            R  * B\(aq + L  * E\(aq  = scale * (-F)
.br

This case (TRANS = \(aqT\(aq) is used to compute an one-norm-based estimate
of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
and (B,E), using DLACON.
.br

If IJOB >= 1, DTGSYL computes a Frobenius norm-based estimate
of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
reciprocal of the smallest singular value of Z. See [1-2] for more
information.
.br

This is a level 3 BLAS algorithm.
.br

.SH ARGUMENTS
.TP 8
TRANS   (input) CHARACTER*1
= \(aqN\(aq, solve the generalized Sylvester equation (1).
= \(aqT\(aq, solve the \(aqtransposed\(aq system (3).
.TP 8
IJOB    (input) INTEGER
Specifies what kind of functionality to be performed.
=0: solve (1) only.
.br
=1: The functionality of 0 and 3.
.br
=2: The functionality of 0 and 4.
.br
=3: Only an estimate of Dif[(A,D), (B,E)] is computed.
(look ahead strategy IJOB  = 1 is used).
=4: Only an estimate of Dif[(A,D), (B,E)] is computed.
( DGECON on sub-systems is used ).
Not referenced if TRANS = \(aqT\(aq.
.TP 8
M       (input) INTEGER
The order of the matrices A and D, and the row dimension of
the matrices C, F, R and L.
.TP 8
N       (input) INTEGER
The order of the matrices B and E, and the column dimension
of the matrices C, F, R and L.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA, M)
The upper quasi triangular matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1, M).
.TP 8
B       (input) DOUBLE PRECISION array, dimension (LDB, N)
The upper quasi triangular matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1, N).
.TP 8
C       (input/output) DOUBLE PRECISION array, dimension (LDC, N)
On entry, C contains the right-hand-side of the first matrix
equation in (1) or (3).
On exit, if IJOB = 0, 1 or 2, C has been overwritten by
the solution R. If IJOB = 3 or 4 and TRANS = \(aqN\(aq, C holds R,
the solution achieved during the computation of the
Dif-estimate.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1, M).
.TP 8
D       (input) DOUBLE PRECISION array, dimension (LDD, M)
The upper triangular matrix D.
.TP 8
LDD     (input) INTEGER
The leading dimension of the array D. LDD >= max(1, M).
.TP 8
E       (input) DOUBLE PRECISION array, dimension (LDE, N)
The upper triangular matrix E.
.TP 8
LDE     (input) INTEGER
The leading dimension of the array E. LDE >= max(1, N).
.TP 8
F       (input/output) DOUBLE PRECISION array, dimension (LDF, N)
On entry, F contains the right-hand-side of the second matrix
equation in (1) or (3).
On exit, if IJOB = 0, 1 or 2, F has been overwritten by
the solution L. If IJOB = 3 or 4 and TRANS = \(aqN\(aq, F holds L,
the solution achieved during the computation of the
Dif-estimate.
.TP 8
LDF     (input) INTEGER
The leading dimension of the array F. LDF >= max(1, M).
.TP 8
DIF     (output) DOUBLE PRECISION
On exit DIF is the reciprocal of a lower bound of the
reciprocal of the Dif-function, i.e. DIF is an upper bound of
Dif[(A,D), (B,E)] = sigma_min(Z), where Z as in (2).
IF IJOB = 0 or TRANS = \(aqT\(aq, DIF is not touched.
.TP 8
SCALE   (output) DOUBLE PRECISION
On exit SCALE is the scaling factor in (1) or (3).
If 0 < SCALE < 1, C and F hold the solutions R and L, resp.,
to a slightly perturbed system but the input matrices A, B, D
and E have not been changed. If SCALE = 0, C and F hold the
solutions R and L, respectively, to the homogeneous system
with C = F = 0. Normally, SCALE = 1.
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK > = 1.
If IJOB = 1 or 2 and TRANS = \(aqN\(aq, LWORK >= max(1,2*M*N).

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
IWORK   (workspace) INTEGER array, dimension (M+N+6)
.TP 8
INFO    (output) INTEGER
=0: successful exit
.br
<0: If INFO = -i, the i-th argument had an illegal value.
.br
>0: (A, D) and (B, E) have common or close eigenvalues.
.SH FURTHER DETAILS
Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
.br

[1] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
    for Solving the Generalized Sylvester Equation and Estimating the
    Separation between Regular Matrix Pairs, Report UMINF - 93.23,
    Department of Computing Science, Umea University, S-901 87 Umea,
    Sweden, December 1993, Revised April 1994, Also as LAPACK Working
    Note 75.  To appear in ACM Trans. on Math. Software, Vol 22,
    No 1, 1996.
.br

[2] B. Kagstrom, A Perturbation Analysis of the Generalized Sylvester
    Equation (AR - LB, DR - LE ) = (C, F), SIAM J. Matrix Anal.
    Appl., 15(4):1045-1060, 1994
.br

[3] B. Kagstrom and L. Westin, Generalized Schur Methods with
    Condition Estimators for Solving the Generalized Sylvester
    Equation, IEEE Transactions on Automatic Control, Vol. 34, No. 7,
    July 1989, pp 745-751.
.br

