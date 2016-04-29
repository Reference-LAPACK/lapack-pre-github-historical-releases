.TH  SLATDF 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLATDF - the LU factorization of the n-by-n matrix Z computed by SGETC2 and computes a contribution to the reciprocal Dif-estimate by solving Z * x = b for x, and choosing the r.h.s
.SH SYNOPSIS
.TP 19
SUBROUTINE SLATDF(
IJOB, N, Z, LDZ, RHS, RDSUM, RDSCAL, IPIV,
JPIV )
.TP 19
.ti +4
INTEGER
IJOB, LDZ, N
.TP 19
.ti +4
REAL
RDSCAL, RDSUM
.TP 19
.ti +4
INTEGER
IPIV( * ), JPIV( * )
.TP 19
.ti +4
REAL
RHS( * ), Z( LDZ, * )
.SH PURPOSE
SLATDF uses the LU factorization of the n-by-n matrix Z computed by
SGETC2 and computes a contribution to the reciprocal Dif-estimate
by solving Z * x = b for x, and choosing the r.h.s. b such that
the norm of x is as large as possible. On entry RHS = b holds the
contribution from earlier solved sub-systems, and on return RHS = x.

The factorization of Z returned by SGETC2 has the form Z = P*L*U*Q,
where P and Q are permutation matrices. L is lower triangular with
unit diagonal elements and U is upper triangular.
.br

.SH ARGUMENTS
.TP 8
IJOB    (input) INTEGER
IJOB = 2: First compute an approximative null-vector e
of Z using SGECON, e is normalized and solve for
Zx = +-e - f with the sign giving the greater value
of 2-norm(x). About 5 times as expensive as Default.
IJOB .ne. 2: Local look ahead strategy where all entries of
the r.h.s. b is choosen as either +1 or -1 (Default).
.TP 8
N       (input) INTEGER
The number of columns of the matrix Z.
.TP 8
Z       (input) REAL array, dimension (LDZ, N)
On entry, the LU part of the factorization of the n-by-n
matrix Z computed by SGETC2:  Z = P * L * U * Q
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDA >= max(1, N).
.TP 8
RHS     (input/output) REAL array, dimension N.
On entry, RHS contains contributions from other subsystems.
On exit, RHS contains the solution of the subsystem with
entries acoording to the value of IJOB (see above).
.TP 8
RDSUM   (input/output) REAL
On entry, the sum of squares of computed contributions to
the Dif-estimate under computation by STGSYL, where the
scaling factor RDSCAL (see below) has been factored out.
On exit, the corresponding sum of squares updated with the
contributions from the current sub-system.
If TRANS = \(aqT\(aq RDSUM is not touched.
NOTE: RDSUM only makes sense when STGSY2 is called by STGSYL.
.TP 8
RDSCAL  (input/output) REAL
On entry, scaling factor used to prevent overflow in RDSUM.
On exit, RDSCAL is updated w.r.t. the current contributions
in RDSUM.
If TRANS = \(aqT\(aq, RDSCAL is not touched.
NOTE: RDSCAL only makes sense when STGSY2 is called by
STGSYL.
.TP 8
IPIV    (input) INTEGER array, dimension (N).
The pivot indices; for 1 <= i <= N, row i of the
matrix has been interchanged with row IPIV(i).
.TP 8
JPIV    (input) INTEGER array, dimension (N).
The pivot indices; for 1 <= j <= N, column j of the
matrix has been interchanged with column JPIV(j).
.SH FURTHER DETAILS
Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
.br

This routine is a further developed implementation of algorithm
BSOLVE in [1] using complete pivoting in the LU factorization.

[1] Bo Kagstrom and Lars Westin,
.br
    Generalized Schur Methods with Condition Estimators for
    Solving the Generalized Sylvester Equation, IEEE Transactions
    on Automatic Control, Vol. 34, No. 7, July 1989, pp 745-751.

[2] Peter Poromaa,
.br
    On Efficient and Robust Estimators for the Separation
    between two Regular Matrix Pairs with Applications in
    Condition Estimation. Report IMINF-95.05, Departement of
    Computing Science, Umea University, S-901 87 Umea, Sweden, 1995.

