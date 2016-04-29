.TH  STGSNA 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
STGSNA - reciprocal condition numbers for specified eigenvalues and/or eigenvectors of a matrix pair (A, B) in generalized real Schur canonical form (or of any matrix pair (Q*A*Z\(aq, Q*B*Z\(aq) with orthogonal matrices Q and Z, where Z\(aq denotes the transpose of Z
.SH SYNOPSIS
.TP 19
SUBROUTINE STGSNA(
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
REAL
A( LDA, * ), B( LDB, * ), DIF( * ), S( * ),
VL( LDVL, * ), VR( LDVR, * ), WORK( * )
.SH PURPOSE
STGSNA estimates reciprocal condition numbers for specified
eigenvalues and/or eigenvectors of a matrix pair (A, B) in
generalized real Schur canonical form (or of any matrix pair
(Q*A*Z\(aq, Q*B*Z\(aq) with orthogonal matrices Q and Z, where
Z\(aq denotes the transpose of Z.

(A, B) must be in generalized real Schur form (as returned by SGGES),
i.e. A is block upper triangular with 1-by-1 and 2-by-2 diagonal
blocks. B is upper triangular.
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
for the eigenpair corresponding to a real eigenvalue w(j),
SELECT(j) must be set to .TRUE.. To select condition numbers
corresponding to a complex conjugate pair of eigenvalues w(j)
and w(j+1), either SELECT(j) or SELECT(j+1) or both, must be
set to .TRUE..
If HOWMNY = \(aqA\(aq, SELECT is not referenced.
.TP 8
N       (input) INTEGER
The order of the square matrix pair (A, B). N >= 0.
.TP 8
A       (input) REAL array, dimension (LDA,N)
The upper quasi-triangular matrix A in the pair (A,B).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
B       (input) REAL array, dimension (LDB,N)
The upper triangular matrix B in the pair (A,B).
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
VL      (input) REAL array, dimension (LDVL,M)
If JOB = \(aqE\(aq or \(aqB\(aq, VL must contain left eigenvectors of
(A, B), corresponding to the eigenpairs specified by HOWMNY
and SELECT. The eigenvectors must be stored in consecutive
columns of VL, as returned by STGEVC.
If JOB = \(aqV\(aq, VL is not referenced.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL. LDVL >= 1.
If JOB = \(aqE\(aq or \(aqB\(aq, LDVL >= N.
.TP 8
VR      (input) REAL array, dimension (LDVR,M)
If JOB = \(aqE\(aq or \(aqB\(aq, VR must contain right eigenvectors of
(A, B), corresponding to the eigenpairs specified by HOWMNY
and SELECT. The eigenvectors must be stored in consecutive
columns ov VR, as returned by STGEVC.
If JOB = \(aqV\(aq, VR is not referenced.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR. LDVR >= 1.
If JOB = \(aqE\(aq or \(aqB\(aq, LDVR >= N.
.TP 8
S       (output) REAL array, dimension (MM)
If JOB = \(aqE\(aq or \(aqB\(aq, the reciprocal condition numbers of the
selected eigenvalues, stored in consecutive elements of the
array. For a complex conjugate pair of eigenvalues two
consecutive elements of S are set to the same value. Thus
S(j), DIF(j), and the j-th columns of VL and VR all
correspond to the same eigenpair (but not in general the
j-th eigenpair, unless all eigenpairs are selected).
If JOB = \(aqV\(aq, S is not referenced.
.TP 8
DIF     (output) REAL array, dimension (MM)
If JOB = \(aqV\(aq or \(aqB\(aq, the estimated reciprocal condition
numbers of the selected eigenvectors, stored in consecutive
elements of the array. For a complex eigenvector two
consecutive elements of DIF are set to the same value. If
the eigenvalues cannot be reordered to compute DIF(j), DIF(j)
is set to 0; this can only occur when the true value would be
very small anyway.
If JOB = \(aqE\(aq, DIF is not referenced.
.TP 8
MM      (input) INTEGER
The number of elements in the arrays S and DIF. MM >= M.
.TP 8
M       (output) INTEGER
The number of elements of the arrays S and DIF used to store
the specified condition numbers; for each selected real
eigenvalue one element is used, and for each selected complex
conjugate pair of eigenvalues, two elements are used.
If HOWMNY = \(aqA\(aq, M is set to N.
.TP 8
WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,N).
If JOB = \(aqV\(aq or \(aqB\(aq LWORK >= 2*N*(N+2)+16.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
IWORK   (workspace) INTEGER array, dimension (N + 6)
If JOB = \(aqE\(aq, IWORK is not referenced.
.TP 8
INFO    (output) INTEGER
=0: Successful exit
.br
<0: If INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The reciprocal of the condition number of a generalized eigenvalue
w = (a, b) is defined as
.br

     S(w) = (|u\(aqAv|**2 + |u\(aqBv|**2)**(1/2) / (norm(u)*norm(v))

where u and v are the left and right eigenvectors of (A, B)
corresponding to w; |z| denotes the absolute value of the complex
number, and norm(u) denotes the 2-norm of the vector u.
.br
The pair (a, b) corresponds to an eigenvalue w = a/b (= u\(aqAv/u\(aqBv)
of the matrix pair (A, B). If both a and b equal zero, then (A B) is
singular and S(I) = -1 is returned.
.br

An approximate error bound on the chordal distance between the i-th
computed generalized eigenvalue w and the corresponding exact
eigenvalue lambda is
.br

     chord(w, lambda) <= EPS * norm(A, B) / S(I)
.br

where EPS is the machine precision.
.br

The reciprocal of the condition number DIF(i) of right eigenvector u
and left eigenvector v corresponding to the generalized eigenvalue w
is defined as follows:
.br

a) If the i-th eigenvalue w = (a,b) is real
.br

   Suppose U and V are orthogonal transformations such that

              U\(aq*(A, B)*V  = (S, T) = ( a   *  ) ( b  *  )  1
                                      ( 0  S22 ),( 0 T22 )  n-1
                                        1  n-1     1 n-1
.br

   Then the reciprocal condition number DIF(i) is
.br

              Difl((a, b), (S22, T22)) = sigma-min( Zl ),

   where sigma-min(Zl) denotes the smallest singular value of the
   2(n-1)-by-2(n-1) matrix
.br

       Zl = [ kron(a, In-1)  -kron(1, S22) ]
.br
            [ kron(b, In-1)  -kron(1, T22) ] .
.br

   Here In-1 is the identity matrix of size n-1. kron(X, Y) is the
   Kronecker product between the matrices X and Y.
.br

   Note that if the default method for computing DIF(i) is wanted
   (see SLATDF), then the parameter DIFDRI (see below) should be
   changed from 3 to 4 (routine SLATDF(IJOB = 2 will be used)).
   See STGSYL for more details.
.br

b) If the i-th and (i+1)-th eigenvalues are complex conjugate pair,

   Suppose U and V are orthogonal transformations such that

              U\(aq*(A, B)*V = (S, T) = ( S11  *   ) ( T11  *  )  2
                                     ( 0    S22 ),( 0    T22) n-2
                                       2    n-2     2    n-2

   and (S11, T11) corresponds to the complex conjugate eigenvalue
   pair (w, conjg(w)). There exist unitary matrices U1 and V1 such
   that
.br

       U1\(aq*S11*V1 = ( s11 s12 )   and U1\(aq*T11*V1 = ( t11 t12 )
                    (  0  s22 )                    (  0  t22 )

   where the generalized eigenvalues w = s11/t11 and
.br
   conjg(w) = s22/t22.
.br

   Then the reciprocal condition number DIF(i) is bounded by

       min( d1, max( 1, |real(s11)/real(s22)| )*d2 )
.br

   where, d1 = Difl((s11, t11), (s22, t22)) = sigma-min(Z1), where
   Z1 is the complex 2-by-2 matrix
.br

            Z1 =  [ s11  -s22 ]
.br
                  [ t11  -t22 ],
.br

   This is done by computing (using real arithmetic) the
.br
   roots of the characteristical polynomial det(Z1\(aq * Z1 - lambda I),
   where Z1\(aq denotes the conjugate transpose of Z1 and det(X) denotes
   the determinant of X.
.br

   and d2 is an upper bound on Difl((S11, T11), (S22, T22)), i.e. an
   upper bound on sigma-min(Z2), where Z2 is (2n-2)-by-(2n-2)

            Z2 = [ kron(S11\(aq, In-2)  -kron(I2, S22) ]
.br
                 [ kron(T11\(aq, In-2)  -kron(I2, T22) ]
.br

   Note that if the default method for computing DIF is wanted (see
   SLATDF), then the parameter DIFDRI (see below) should be changed
   from 3 to 4 (routine SLATDF(IJOB = 2 will be used)). See STGSYL
   for more details.
.br

For each eigenvalue/vector specified by SELECT, DIF stores a
Frobenius norm-based estimate of Difl.
.br

An approximate error bound for the i-th computed eigenvector VL(i) or
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
    Estimation: Theory, Algorithms and Software,
.br
    Report UMINF - 94.04, Department of Computing Science, Umea
    University, S-901 87 Umea, Sweden, 1994. Also as LAPACK Working
    Note 87. To appear in Numerical Algorithms, 1996.
.br

[3] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
    for Solving the Generalized Sylvester Equation and Estimating the
    Separation between Regular Matrix Pairs, Report UMINF - 93.23,
    Department of Computing Science, Umea University, S-901 87 Umea,
    Sweden, December 1993, Revised April 1994, Also as LAPACK Working
    Note 75.  To appear in ACM Trans. on Math. Software, Vol 22,
    No 1, 1996.
.br

