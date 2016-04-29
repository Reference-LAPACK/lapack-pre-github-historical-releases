.TH  ZTRSNA 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTRSNA - reciprocal condition numbers for specified eigenvalues and/or right eigenvectors of a complex upper triangular matrix T (or of any matrix Q*T*Q**H with Q unitary)
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTRSNA(
JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
LDVR, S, SEP, MM, M, WORK, LDWORK, RWORK,
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
DOUBLE
PRECISION RWORK( * ), S( * ), SEP( * )
.TP 19
.ti +4
COMPLEX*16
T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
WORK( LDWORK, * )
.SH PURPOSE
ZTRSNA estimates reciprocal condition numbers for specified
eigenvalues and/or right eigenvectors of a complex upper triangular
matrix T (or of any matrix Q*T*Q**H with Q unitary).

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
for the j-th eigenpair, SELECT(j) must be set to .TRUE..
If HOWMNY = \(aqA\(aq, SELECT is not referenced.
.TP 8
N       (input) INTEGER
The order of the matrix T. N >= 0.
.TP 8
T       (input) COMPLEX*16 array, dimension (LDT,N)
The upper triangular matrix T.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= max(1,N).
.TP 8
VL      (input) COMPLEX*16 array, dimension (LDVL,M)
If JOB = \(aqE\(aq or \(aqB\(aq, VL must contain left eigenvectors of T
(or of any Q*T*Q**H with Q unitary), corresponding to the
eigenpairs specified by HOWMNY and SELECT. The eigenvectors
must be stored in consecutive columns of VL, as returned by
ZHSEIN or ZTREVC.
If JOB = \(aqV\(aq, VL is not referenced.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL.
LDVL >= 1; and if JOB = \(aqE\(aq or \(aqB\(aq, LDVL >= N.
.TP 8
VR      (input) COMPLEX*16 array, dimension (LDVR,M)
If JOB = \(aqE\(aq or \(aqB\(aq, VR must contain right eigenvectors of T
(or of any Q*T*Q**H with Q unitary), corresponding to the
eigenpairs specified by HOWMNY and SELECT. The eigenvectors
must be stored in consecutive columns of VR, as returned by
ZHSEIN or ZTREVC.
If JOB = \(aqV\(aq, VR is not referenced.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.
LDVR >= 1; and if JOB = \(aqE\(aq or \(aqB\(aq, LDVR >= N.
.TP 8
S       (output) DOUBLE PRECISION array, dimension (MM)
If JOB = \(aqE\(aq or \(aqB\(aq, the reciprocal condition numbers of the
selected eigenvalues, stored in consecutive elements of the
array. Thus S(j), SEP(j), and the j-th columns of VL and VR
all correspond to the same eigenpair (but not in general the
j-th eigenpair, unless all eigenpairs are selected).
If JOB = \(aqV\(aq, S is not referenced.
.TP 8
SEP     (output) DOUBLE PRECISION array, dimension (MM)
If JOB = \(aqV\(aq or \(aqB\(aq, the estimated reciprocal condition
numbers of the selected eigenvectors, stored in consecutive
elements of the array.
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
WORK    (workspace) COMPLEX*16 array, dimension (LDWORK,N+6)
If JOB = \(aqE\(aq, WORK is not referenced.
.TP 8
LDWORK  (input) INTEGER
The leading dimension of the array WORK.
LDWORK >= 1; and if JOB = \(aqV\(aq or \(aqB\(aq, LDWORK >= N.
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
If JOB = \(aqE\(aq, RWORK is not referenced.
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
to lambda; v\(aq denotes the conjugate transpose of v, and norm(u)
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

