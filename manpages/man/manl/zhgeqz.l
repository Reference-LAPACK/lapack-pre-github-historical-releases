.TH  ZHGEQZ 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZHGEQZ - the eigenvalues of a complex matrix pair (H,T),
.SH SYNOPSIS
.TP 19
SUBROUTINE ZHGEQZ(
JOB, COMPQ, COMPZ, N, ILO, IHI, H, LDH, T, LDT,
ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,
RWORK, INFO )
.TP 19
.ti +4
CHARACTER
COMPQ, COMPZ, JOB
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDH, LDQ, LDT, LDZ, LWORK, N
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * )
.TP 19
.ti +4
COMPLEX*16
ALPHA( * ), BETA( * ), H( LDH, * ),
Q( LDQ, * ), T( LDT, * ), WORK( * ),
Z( LDZ, * )
.SH PURPOSE
ZHGEQZ computes the eigenvalues of a complex matrix pair (H,T),
where H is an upper Hessenberg matrix and T is upper triangular,
using the single-shift QZ method.
.br
Matrix pairs of this type are produced by the reduction to
generalized upper Hessenberg form of a complex matrix pair (A,B):

.br
   A = Q1*H*Z1**H,  B = Q1*T*Z1**H,
.br

.br
as computed by ZGGHRD.
.br

.br
If JOB=\(aqS\(aq, then the Hessenberg-triangular pair (H,T) is
.br
also reduced to generalized Schur form,
.br

.br
   H = Q*S*Z**H,  T = Q*P*Z**H,
.br

.br
where Q and Z are unitary matrices and S and P are upper triangular.

.br
Optionally, the unitary matrix Q from the generalized Schur
factorization may be postmultiplied into an input matrix Q1, and the
unitary matrix Z may be postmultiplied into an input matrix Z1.
If Q1 and Z1 are the unitary matrices from ZGGHRD that reduced
the matrix pair (A,B) to generalized Hessenberg form, then the output
matrices Q1*Q and Z1*Z are the unitary factors from the generalized
Schur factorization of (A,B):
.br

.br
   A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H.
.br

.br
To avoid overflow, eigenvalues of the matrix pair (H,T)
.br
(equivalently, of (A,B)) are computed as a pair of complex values
(alpha,beta).  If beta is nonzero, lambda = alpha / beta is an
eigenvalue of the generalized nonsymmetric eigenvalue problem (GNEP)
   A*x = lambda*B*x
.br
and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
alternate form of the GNEP
.br
   mu*A*y = B*y.
.br
The values of alpha and beta for the i-th eigenvalue can be read
directly from the generalized Schur form:  alpha = S(i,i),
beta = P(i,i).
.br

Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix
     Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     pp. 241--256.
.br

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
= \(aqE\(aq: Compute eigenvalues only;
.br
= \(aqS\(aq: Computer eigenvalues and the Schur form.
.TP 8
COMPQ   (input) CHARACTER*1
.br
= \(aqN\(aq: Left Schur vectors (Q) are not computed;
.br
= \(aqI\(aq: Q is initialized to the unit matrix and the matrix Q
of left Schur vectors of (H,T) is returned;
= \(aqV\(aq: Q must contain a unitary matrix Q1 on entry and
the product Q1*Q is returned.
.TP 8
COMPZ   (input) CHARACTER*1
= \(aqN\(aq: Right Schur vectors (Z) are not computed;
.br
= \(aqI\(aq: Q is initialized to the unit matrix and the matrix Z
of right Schur vectors of (H,T) is returned;
= \(aqV\(aq: Z must contain a unitary matrix Z1 on entry and
the product Z1*Z is returned.
.TP 8
N       (input) INTEGER
The order of the matrices H, T, Q, and Z.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
ILO and IHI mark the rows and columns of H which are in
Hessenberg form.  It is assumed that A is already upper
triangular in rows and columns 1:ILO-1 and IHI+1:N.
If N > 0, 1 <= ILO <= IHI <= N; if N = 0, ILO=1 and IHI=0.
.TP 8
H       (input/output) COMPLEX*16 array, dimension (LDH, N)
On entry, the N-by-N upper Hessenberg matrix H.
On exit, if JOB = \(aqS\(aq, H contains the upper triangular
matrix S from the generalized Schur factorization.
If JOB = \(aqE\(aq, the diagonal of H matches that of S, but
the rest of H is unspecified.
.TP 8
LDH     (input) INTEGER
The leading dimension of the array H.  LDH >= max( 1, N ).
.TP 8
T       (input/output) COMPLEX*16 array, dimension (LDT, N)
On entry, the N-by-N upper triangular matrix T.
On exit, if JOB = \(aqS\(aq, T contains the upper triangular
matrix P from the generalized Schur factorization.
If JOB = \(aqE\(aq, the diagonal of T matches that of P, but
the rest of T is unspecified.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T.  LDT >= max( 1, N ).
.TP 8
ALPHA   (output) COMPLEX*16 array, dimension (N)
The complex scalars alpha that define the eigenvalues of
GNEP.  ALPHA(i) = S(i,i) in the generalized Schur
factorization.
.TP 8
BETA    (output) COMPLEX*16 array, dimension (N)
The real non-negative scalars beta that define the
eigenvalues of GNEP.  BETA(i) = P(i,i) in the generalized
Schur factorization.

Together, the quantities alpha = ALPHA(j) and beta = BETA(j)
represent the j-th eigenvalue of the matrix pair (A,B), in
one of the forms lambda = alpha/beta or mu = beta/alpha.
Since either lambda or mu may overflow, they should not,
in general, be computed.
.TP 8
Q       (input/output) COMPLEX*16 array, dimension (LDQ, N)
On entry, if COMPZ = \(aqV\(aq, the unitary matrix Q1 used in the
reduction of (A,B) to generalized Hessenberg form.
On exit, if COMPZ = \(aqI\(aq, the unitary matrix of left Schur
vectors of (H,T), and if COMPZ = \(aqV\(aq, the unitary matrix of
left Schur vectors of (A,B).
Not referenced if COMPZ = \(aqN\(aq.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  LDQ >= 1.
If COMPQ=\(aqV\(aq or \(aqI\(aq, then LDQ >= N.
.TP 8
Z       (input/output) COMPLEX*16 array, dimension (LDZ, N)
On entry, if COMPZ = \(aqV\(aq, the unitary matrix Z1 used in the
reduction of (A,B) to generalized Hessenberg form.
On exit, if COMPZ = \(aqI\(aq, the unitary matrix of right Schur
vectors of (H,T), and if COMPZ = \(aqV\(aq, the unitary matrix of
right Schur vectors of (A,B).
Not referenced if COMPZ = \(aqN\(aq.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1.
If COMPZ=\(aqV\(aq or \(aqI\(aq, then LDZ >= N.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO >= 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,N).

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
= 1,...,N: the QZ iteration did not converge.  (H,T) is not
in Schur form, but ALPHA(i) and BETA(i),
i=INFO+1,...,N should be correct.
= N+1,...,2*N: the shift calculation failed.  (H,T) is not
in Schur form, but ALPHA(i) and BETA(i),
i=INFO-N+1,...,N should be correct.
.SH FURTHER DETAILS
We assume that complex ABS works as long as its value is less than
overflow.
.br

