.TH  SHGEQZ 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SHGEQZ - the eigenvalues of a real matrix pair (H,T),
.SH SYNOPSIS
.TP 19
SUBROUTINE SHGEQZ(
JOB, COMPQ, COMPZ, N, ILO, IHI, H, LDH, T, LDT,
ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,
LWORK, INFO )
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
REAL
ALPHAI( * ), ALPHAR( * ), BETA( * ),
H( LDH, * ), Q( LDQ, * ), T( LDT, * ),
WORK( * ), Z( LDZ, * )
.SH PURPOSE
SHGEQZ computes the eigenvalues of a real matrix pair (H,T),
where H is an upper Hessenberg matrix and T is upper triangular,
using the double-shift QZ method.
.br
Matrix pairs of this type are produced by the reduction to
generalized upper Hessenberg form of a real matrix pair (A,B):

   A = Q1*H*Z1**T,  B = Q1*T*Z1**T,
.br

as computed by SGGHRD.
.br

If JOB=\(aqS\(aq, then the Hessenberg-triangular pair (H,T) is
.br
also reduced to generalized Schur form,
.br

.br
   H = Q*S*Z**T,  T = Q*P*Z**T,
.br

.br
where Q and Z are orthogonal matrices, P is an upper triangular
matrix, and S is a quasi-triangular matrix with 1-by-1 and 2-by-2
diagonal blocks.
.br

The 1-by-1 blocks correspond to real eigenvalues of the matrix pair
(H,T) and the 2-by-2 blocks correspond to complex conjugate pairs of
eigenvalues.
.br

Additionally, the 2-by-2 upper triangular diagonal blocks of P
corresponding to 2-by-2 blocks of S are reduced to positive diagonal
form, i.e., if S(j+1,j) is non-zero, then P(j+1,j) = P(j,j+1) = 0,
P(j,j) > 0, and P(j+1,j+1) > 0.
.br

Optionally, the orthogonal matrix Q from the generalized Schur
factorization may be postmultiplied into an input matrix Q1, and the
orthogonal matrix Z may be postmultiplied into an input matrix Z1.
If Q1 and Z1 are the orthogonal matrices from SGGHRD that reduced
the matrix pair (A,B) to generalized upper Hessenberg form, then the
output matrices Q1*Q and Z1*Z are the orthogonal factors from the
generalized Schur factorization of (A,B):
.br

   A = (Q1*Q)*S*(Z1*Z)**T,  B = (Q1*Q)*P*(Z1*Z)**T.
.br

.br
To avoid overflow, eigenvalues of the matrix pair (H,T) (equivalently,
of (A,B)) are computed as a pair of values (alpha,beta), where alpha is
complex and beta real.
.br
If beta is nonzero, lambda = alpha / beta is an eigenvalue of the
generalized nonsymmetric eigenvalue problem (GNEP)
.br
   A*x = lambda*B*x
.br
and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
alternate form of the GNEP
.br
   mu*A*y = B*y.
.br
Real eigenvalues can be read directly from the generalized Schur
form: 
.br
  alpha = S(i,i), beta = P(i,i).
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
= \(aqS\(aq: Compute eigenvalues and the Schur form. 
.TP 8
COMPQ   (input) CHARACTER*1
.br
= \(aqN\(aq: Left Schur vectors (Q) are not computed;
.br
= \(aqI\(aq: Q is initialized to the unit matrix and the matrix Q
of left Schur vectors of (H,T) is returned;
= \(aqV\(aq: Q must contain an orthogonal matrix Q1 on entry and
the product Q1*Q is returned.
.TP 8
COMPZ   (input) CHARACTER*1
= \(aqN\(aq: Right Schur vectors (Z) are not computed;
.br
= \(aqI\(aq: Z is initialized to the unit matrix and the matrix Z
of right Schur vectors of (H,T) is returned;
= \(aqV\(aq: Z must contain an orthogonal matrix Z1 on entry and
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
H       (input/output) REAL array, dimension (LDH, N)
On entry, the N-by-N upper Hessenberg matrix H.
On exit, if JOB = \(aqS\(aq, H contains the upper quasi-triangular
matrix S from the generalized Schur factorization;
2-by-2 diagonal blocks (corresponding to complex conjugate
pairs of eigenvalues) are returned in standard form, with
H(i,i) = H(i+1,i+1) and H(i+1,i)*H(i,i+1) < 0.
If JOB = \(aqE\(aq, the diagonal blocks of H match those of S, but
the rest of H is unspecified.
.TP 8
LDH     (input) INTEGER
The leading dimension of the array H.  LDH >= max( 1, N ).
.TP 8
T       (input/output) REAL array, dimension (LDT, N)
On entry, the N-by-N upper triangular matrix T.
On exit, if JOB = \(aqS\(aq, T contains the upper triangular
matrix P from the generalized Schur factorization;
2-by-2 diagonal blocks of P corresponding to 2-by-2 blocks of S
are reduced to positive diagonal form, i.e., if H(j+1,j) is
non-zero, then T(j+1,j) = T(j,j+1) = 0, T(j,j) > 0, and
T(j+1,j+1) > 0.
If JOB = \(aqE\(aq, the diagonal blocks of T match those of P, but
the rest of T is unspecified.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T.  LDT >= max( 1, N ).
.TP 8
ALPHAR  (output) REAL array, dimension (N)
The real parts of each scalar alpha defining an eigenvalue
of GNEP.
.TP 8
ALPHAI  (output) REAL array, dimension (N)
The imaginary parts of each scalar alpha defining an
eigenvalue of GNEP.
If ALPHAI(j) is zero, then the j-th eigenvalue is real; if
positive, then the j-th and (j+1)-st eigenvalues are a
complex conjugate pair, with ALPHAI(j+1) = -ALPHAI(j).
.TP 8
BETA    (output) REAL array, dimension (N)
The scalars beta that define the eigenvalues of GNEP.
Together, the quantities alpha = (ALPHAR(j),ALPHAI(j)) and
beta = BETA(j) represent the j-th eigenvalue of the matrix
pair (A,B), in one of the forms lambda = alpha/beta or
mu = beta/alpha.  Since either lambda or mu may overflow,
they should not, in general, be computed.
.TP 8
Q       (input/output) REAL array, dimension (LDQ, N)
On entry, if COMPZ = \(aqV\(aq, the orthogonal matrix Q1 used in
the reduction of (A,B) to generalized Hessenberg form.
On exit, if COMPZ = \(aqI\(aq, the orthogonal matrix of left Schur
vectors of (H,T), and if COMPZ = \(aqV\(aq, the orthogonal matrix
of left Schur vectors of (A,B).
Not referenced if COMPZ = \(aqN\(aq.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  LDQ >= 1.
If COMPQ=\(aqV\(aq or \(aqI\(aq, then LDQ >= N.
.TP 8
Z       (input/output) REAL array, dimension (LDZ, N)
On entry, if COMPZ = \(aqV\(aq, the orthogonal matrix Z1 used in
the reduction of (A,B) to generalized Hessenberg form.
On exit, if COMPZ = \(aqI\(aq, the orthogonal matrix of
right Schur vectors of (H,T), and if COMPZ = \(aqV\(aq, the
orthogonal matrix of right Schur vectors of (A,B).
Not referenced if COMPZ = \(aqN\(aq.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1.
If COMPZ=\(aqV\(aq or \(aqI\(aq, then LDZ >= N.
.TP 8
WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
On exit, if INFO >= 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,N).

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
= 1,...,N: the QZ iteration did not converge.  (H,T) is not
in Schur form, but ALPHAR(i), ALPHAI(i), and
BETA(i), i=INFO+1,...,N should be correct.
= N+1,...,2*N: the shift calculation failed.  (H,T) is not
in Schur form, but ALPHAR(i), ALPHAI(i), and
BETA(i), i=INFO-N+1,...,N should be correct.
.SH FURTHER DETAILS
Iteration counters:
.br

JITER  -- counts iterations.
.br
IITER  -- counts iterations run since ILAST was last
.br
          changed.  This is therefore reset only when a 1-by-1 or
          2-by-2 block deflates off the bottom.
.br

