.TH  STRSNA 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
STRSNA - reciprocal condition numbers for specified eigenvalues and/or right eigenvectors of a real upper quasi-triangular matrix T (or of any matrix Q*T*Q**T with Q orthogonal)
.SH SYNOPSIS
.TP 19
SUBROUTINE STRSNA(
JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
LDVR, S, SEP, MM, M, WORK, LDWORK, IWORK,
INFO )
.TP 19
.ti +4
CHARACTER
HOWMNY, JOB
.TP 19
.ti +4
INTEGER
INFO, LDT, LDVL, LDVR, LDWORK, M, MM, N
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
S( * ), SEP( * ), T( LDT, * ), VL( LDVL, * ),
VR( LDVR, * ), WORK( LDWORK, * )
.SH PURPOSE
STRSNA estimates reciprocal condition numbers for specified
eigenvalues and/or right eigenvectors of a real upper
quasi-triangular matrix T (or of any matrix Q*T*Q**T with Q
orthogonal).

T must be in Schur canonical form (as returned by SHSEQR), that is,
block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
2-by-2 diagonal block has its diagonal elements equal and its
off-diagonal elements of opposite sign.
.br

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
Specifies whether condition numbers are required for
eigenvalues (S) or eigenvectors (SEP):
.br
= \(aqE\(aq: for eigenvalues only (S);
.br
= \(aqV\(aq: for eigenvectors only (SEP);
.br
= \(aqB\(aq: for both eigenvalues and eigenvectors (S and SEP).
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
The order of the matrix T. N >= 0.
.TP 8
T       (input) REAL array, dimension (LDT,N)
The upper quasi-triangular matrix T, in Schur canonical form.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= max(1,N).
.TP 8
VL      (input) REAL array, dimension (LDVL,M)
If JOB = \(aqE\(aq or \(aqB\(aq, VL must contain left eigenvectors of T
(or of any Q*T*Q**T with Q orthogonal), corresponding to the
eigenpairs specified by HOWMNY and SELECT. The eigenvectors
must be stored in consecutive columns of VL, as returned by
SHSEIN or STREVC.
If JOB = \(aqV\(aq, VL is not referenced.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL.
LDVL >= 1; and if JOB = \(aqE\(aq or \(aqB\(aq, LDVL >= N.
.TP 8
VR      (input) REAL array, dimension (LDVR,M)
If JOB = \(aqE\(aq or \(aqB\(aq, VR must contain right eigenvectors of T
(or of any Q*T*Q**T with Q orthogonal), corresponding to the
eigenpairs specified by HOWMNY and SELECT. The eigenvectors
must be stored in consecutive columns of VR, as returned by
SHSEIN or STREVC.
If JOB = \(aqV\(aq, VR is not referenced.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.
LDVR >= 1; and if JOB = \(aqE\(aq or \(aqB\(aq, LDVR >= N.
.TP 8
S       (output) REAL array, dimension (MM)
If JOB = \(aqE\(aq or \(aqB\(aq, the reciprocal condition numbers of the
selected eigenvalues, stored in consecutive elements of the
array. For a complex conjugate pair of eigenvalues two
consecutive elements of S are set to the same value. Thus
S(j), SEP(j), and the j-th columns of VL and VR all
correspond to the same eigenpair (but not in general the
j-th eigenpair, unless all eigenpairs are selected).
If JOB = \(aqV\(aq, S is not referenced.
.TP 8
SEP     (output) REAL array, dimension (MM)
If JOB = \(aqV\(aq or \(aqB\(aq, the estimated reciprocal condition
numbers of the selected eigenvectors, stored in consecutive
elements of the array. For a complex eigenvector two
consecutive elements of SEP are set to the same value. If
the eigenvalues cannot be reordered to compute SEP(j), SEP(j)
is set to 0; this can only occur when the true value would be
very small anyway.
If JOB = \(aqE\(aq, SEP is not referenced.
.TP 8
MM      (input) INTEGER
The number of elements in the arrays S (if JOB = \(aqE\(aq or \(aqB\(aq)
and/or SEP (if JOB = \(aqV\(aq or \(aqB\(aq). MM >= M.
.TP 8
M       (output) INTEGER
The number of elements of the arrays S and/or SEP actually
used to store the estimated condition numbers.
If HOWMNY = \(aqA\(aq, M is set to N.
.TP 8
WORK    (workspace) REAL array, dimension (LDWORK,N+6)
If JOB = \(aqE\(aq, WORK is not referenced.
.TP 8
LDWORK  (input) INTEGER
The leading dimension of the array WORK.
LDWORK >= 1; and if JOB = \(aqV\(aq or \(aqB\(aq, LDWORK >= N.
.TP 8
IWORK   (workspace) INTEGER array, dimension (2*(N-1))
If JOB = \(aqE\(aq, IWORK is not referenced.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The reciprocal of the condition number of an eigenvalue lambda is
defined as
.br

        S(lambda) = |v\(aq*u| / (norm(u)*norm(v))
.br

where u and v are the right and left eigenvectors of T corresponding
to lambda; v\(aq denotes the conjugate-transpose of v, and norm(u)
denotes the Euclidean norm. These reciprocal condition numbers always
lie between zero (very badly conditioned) and one (very well
conditioned). If n = 1, S(lambda) is defined to be 1.
.br

An approximate error bound for a computed eigenvalue W(i) is given by

                    EPS * norm(T) / S(i)
.br

where EPS is the machine precision.
.br

The reciprocal of the condition number of the right eigenvector u
corresponding to lambda is defined as follows. Suppose
.br

            T = ( lambda  c  )
.br
                (   0    T22 )
.br

Then the reciprocal condition number is
.br

        SEP( lambda, T22 ) = sigma-min( T22 - lambda*I )
.br

where sigma-min denotes the smallest singular value. We approximate
the smallest singular value by the reciprocal of an estimate of the
one-norm of the inverse of T22 - lambda*I. If n = 1, SEP(1) is
defined to be abs(T(1,1)).
.br

An approximate error bound for a computed right eigenvector VR(i)
is given by
.br

                    EPS * norm(T) / SEP(i)
.br

