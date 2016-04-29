.TH  ZTGSNA 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTGSNA - reciprocal condition numbers for specified eigenvalues and/or eigenvectors of a matrix pair (A, B)
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTGSNA(
JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,
LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,
IWORK, INFO )
.TP 19
.ti +4
CHARACTER
HOWMNY, JOB
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LDVL, LDVR, LWORK, M, MM, N
.TP 19
.ti +4
LOGICAL
SELECT( * )
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION DIF( * ), S( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), VL( LDVL, * ),
VR( LDVR, * ), WORK( * )
.SH PURPOSE
ZTGSNA estimates reciprocal condition numbers for specified
eigenvalues and/or eigenvectors of a matrix pair (A, B).

(A, B) must be in generalized Schur canonical form, that is, A and
B are both upper triangular.
.br

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
Specifies whether condition numbers are required for
eigenvalues (S) or eigenvectors (DIF):
.br
= \(aqE\(aq: for eigenvalues only (S);
.br
= \(aqV\(aq: for eigenvectors only (DIF);
.br
= \(aqB\(aq: for both eigenvalues and eigenvectors (S and DIF).
.TP 8
HOWMNY  (input) CHARACTER*1
.br
= \(aqA\(aq: compute condition numbers for all eigenpairs;
.br
= \(aqS\(aq: compute condition numbers for selected eigenpairs
specified by the array SELECT.
.TP 8
SELECT  (input) LOGICAL array, dimension (N)
If HOWMNY = \(aqS\(aq, SELECT specifies the eigenpairs for which
condition numbers are required. To select condition numbers
for the corresponding j-th eigenvalue and/or eigenvector,
SELECT(j) must be set to .TRUE..
If HOWMNY = \(aqA\(aq, SELECT is not referenced.
.TP 8
N       (input) INTEGER
The order of the square matrix pair (A, B). N >= 0.
.TP 8
A       (input) COMPLEX*16 array, dimension (LDA,N)
The upper triangular matrix A in the pair (A,B).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
B       (input) COMPLEX*16 array, dimension (LDB,N)
The upper triangular matrix B in the pair (A, B).
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
VL      (input) COMPLEX*16 array, dimension (LDVL,M)
IF JOB = \(aqE\(aq or \(aqB\(aq, VL must contain left eigenvectors of
(A, B), corresponding to the eigenpairs specified by HOWMNY
and SELECT.  The eigenvectors must be stored in consecutive
columns of VL, as returned by ZTGEVC.
If JOB = \(aqV\(aq, VL is not referenced.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL. LDVL >= 1; and
If JOB = \(aqE\(aq or \(aqB\(aq, LDVL >= N.
.TP 8
VR      (input) COMPLEX*16 array, dimension (LDVR,M)
IF JOB = \(aqE\(aq or \(aqB\(aq, VR must contain right eigenvectors of
(A, B), corresponding to the eigenpairs specified by HOWMNY
and SELECT.  The eigenvectors must be stored in consecutive
columns of VR, as returned by ZTGEVC.
If JOB = \(aqV\(aq, VR is not referenced.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR. LDVR >= 1;
If JOB = \(aqE\(aq or \(aqB\(aq, LDVR >= N.
.TP 8
S       (output) DOUBLE PRECISION array, dimension (MM)
If JOB = \(aqE\(aq or \(aqB\(aq, the reciprocal condition numbers of the
selected eigenvalues, stored in consecutive elements of the
array.
If JOB = \(aqV\(aq, S is not referenced.
.TP 8
DIF     (output) DOUBLE PRECISION array, dimension (MM)
If JOB = \(aqV\(aq or \(aqB\(aq, the estimated reciprocal condition
numbers of the selected eigenvectors, stored in consecutive
elements of the array.
If the eigenvalues cannot be reordered to compute DIF(j),
DIF(j) is set to 0; this can only occur when the true value
would be very small anyway.
For each eigenvalue/vector specified by SELECT, DIF stores
a Frobenius norm-based estimate of Difl.
If JOB = \(aqE\(aq, DIF is not referenced.
.TP 8
MM      (input) INTEGER
The number of elements in the arrays S and DIF. MM >= M.
.TP 8
M       (output) INTEGER
The number of elements of the arrays S and DIF used to store
the specified condition numbers; for each selected eigenvalue
one element is used. If HOWMNY = \(aqA\(aq, M is set to N.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 7
LWORK  (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,N).
If JOB = \(aqV\(aq or \(aqB\(aq, LWORK >= max(1,2*N*N).
.TP 8
IWORK   (workspace) INTEGER array, dimension (N+2)
If JOB = \(aqE\(aq, IWORK is not referenced.
.TP 8
INFO    (output) INTEGER
= 0: Successful exit
.br
< 0: If INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The reciprocal of the condition number of the i-th generalized
eigenvalue w = (a, b) is defined as
.br

        S(I) = (|v\(aqAu|**2 + |v\(aqBu|**2)**(1/2) / (norm(u)*norm(v))

where u and v are the right and left eigenvectors of (A, B)
corresponding to w; |z| denotes the absolute value of the complex
number, and norm(u) denotes the 2-norm of the vector u. The pair
(a, b) corresponds to an eigenvalue w = a/b (= v\(aqAu/v\(aqBu) of the
matrix pair (A, B). If both a and b equal zero, then (A,B) is
singular and S(I) = -1 is returned.
.br

An approximate error bound on the chordal distance between the i-th
computed generalized eigenvalue w and the corresponding exact
eigenvalue lambda is
.br

        chord(w, lambda) <=   EPS * norm(A, B) / S(I),
.br

where EPS is the machine precision.
.br

The reciprocal of the condition number of the right eigenvector u
and left eigenvector v corresponding to the generalized eigenvalue w
is defined as follows. Suppose
.br

                 (A, B) = ( a   *  ) ( b  *  )  1
.br
                          ( 0  A22 ),( 0 B22 )  n-1
.br
                            1  n-1     1 n-1
.br

Then the reciprocal condition number DIF(I) is
.br

        Difl[(a, b), (A22, B22)]  = sigma-min( Zl )
.br

where sigma-min(Zl) denotes the smallest singular value of

       Zl = [ kron(a, In-1) -kron(1, A22) ]
.br
            [ kron(b, In-1) -kron(1, B22) ].
.br

Here In-1 is the identity matrix of size n-1 and X\(aq is the conjugate
transpose of X. kron(X, Y) is the Kronecker product between the
matrices X and Y.
.br

We approximate the smallest singular value of Zl with an upper
bound. This is done by ZLATDF.
.br

An approximate error bound for a computed eigenvector VL(i) or
VR(i) is given by
.br

                    EPS * norm(A, B) / DIF(i).
.br

See ref. [2-3] for more details and further references.
.br

Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
.br

References
.br
==========
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
    Sweden, December 1993, Revised April 1994, Also as LAPACK Working
    Note 75.
.br
    To appear in ACM Trans. on Math. Software, Vol 22, No 1, 1996.

