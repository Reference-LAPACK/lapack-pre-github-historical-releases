.TH  ZTGSJA 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTGSJA - the generalized singular value decomposition (GSVD) of two complex upper triangular (or trapezoidal) matrices A and B
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTGSJA(
JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,
LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,
Q, LDQ, WORK, NCYCLE, INFO )
.TP 19
.ti +4
CHARACTER
JOBQ, JOBU, JOBV
.TP 19
.ti +4
INTEGER
INFO, K, L, LDA, LDB, LDQ, LDU, LDV, M, N,
NCYCLE, P
.TP 19
.ti +4
DOUBLE
PRECISION TOLA, TOLB
.TP 19
.ti +4
DOUBLE
PRECISION ALPHA( * ), BETA( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
U( LDU, * ), V( LDV, * ), WORK( * )
.SH PURPOSE
ZTGSJA computes the generalized singular value decomposition (GSVD)
of two complex upper triangular (or trapezoidal) matrices A and B.

On entry, it is assumed that matrices A and B have the following
forms, which may be obtained by the preprocessing subroutine ZGGSVP
from a general M-by-N matrix A and P-by-N matrix B:
.br

             N-K-L  K    L
.br
   A =    K ( 0    A12  A13 ) if M-K-L >= 0;
.br
          L ( 0     0   A23 )
.br
      M-K-L ( 0     0    0  )
.br

           N-K-L  K    L
.br
   A =  K ( 0    A12  A13 ) if M-K-L < 0;
.br
      M-K ( 0     0   A23 )
.br

           N-K-L  K    L
.br
   B =  L ( 0     0   B13 )
.br
      P-L ( 0     0    0  )
.br

where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0,
otherwise A23 is (M-K)-by-L upper trapezoidal.
.br

On exit,
.br

       U\(aq*A*Q = D1*( 0 R ),    V\(aq*B*Q = D2*( 0 R ),
.br

where U, V and Q are unitary matrices, Z\(aq denotes the conjugate
transpose of Z, R is a nonsingular upper triangular matrix, and D1
and D2 are ``diagonal\(aq\(aq matrices, which are of the following
structures:
.br

If M-K-L >= 0,
.br

                    K  L
.br
       D1 =     K ( I  0 )
.br
                L ( 0  C )
.br
            M-K-L ( 0  0 )
.br

                   K  L
.br
       D2 = L   ( 0  S )
.br
            P-L ( 0  0 )
.br

               N-K-L  K    L
.br
  ( 0 R ) = K (  0   R11  R12 ) K
.br
            L (  0    0   R22 ) L
.br

where
.br

  C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
.br
  S = diag( BETA(K+1),  ... , BETA(K+L) ),
.br
  C**2 + S**2 = I.
.br

  R is stored in A(1:K+L,N-K-L+1:N) on exit.
.br

If M-K-L < 0,
.br

               K M-K K+L-M
.br
    D1 =   K ( I  0    0   )
.br
         M-K ( 0  C    0   )
.br

                 K M-K K+L-M
.br
    D2 =   M-K ( 0  S    0   )
.br
         K+L-M ( 0  0    I   )
.br
           P-L ( 0  0    0   )
.br

               N-K-L  K   M-K  K+L-M
.br

          M-K ( 0     0   R22  R23  )
.br
        K+L-M ( 0     0    0   R33  )
.br

where
.br
C = diag( ALPHA(K+1), ... , ALPHA(M) ),
.br
S = diag( BETA(K+1),  ... , BETA(M) ),
.br
C**2 + S**2 = I.
.br

R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
    (  0  R22 R23 )
.br
in B(M-K+1:L,N+M-K-L+1:N) on exit.
.br

The computation of the unitary transformation matrices U, V or Q
is optional.  These matrices may either be formed explicitly, or they
may be postmultiplied into input matrices U1, V1, or Q1.
.br

.SH ARGUMENTS
.TP 8
JOBU    (input) CHARACTER*1
= \(aqU\(aq:  U must contain a unitary matrix U1 on entry, and
the product U1*U is returned;
= \(aqI\(aq:  U is initialized to the unit matrix, and the
unitary matrix U is returned;
= \(aqN\(aq:  U is not computed.
.TP 8
JOBV    (input) CHARACTER*1
.br
= \(aqV\(aq:  V must contain a unitary matrix V1 on entry, and
the product V1*V is returned;
= \(aqI\(aq:  V is initialized to the unit matrix, and the
unitary matrix V is returned;
= \(aqN\(aq:  V is not computed.
.TP 8
JOBQ    (input) CHARACTER*1
.br
= \(aqQ\(aq:  Q must contain a unitary matrix Q1 on entry, and
the product Q1*Q is returned;
= \(aqI\(aq:  Q is initialized to the unit matrix, and the
unitary matrix Q is returned;
= \(aqN\(aq:  Q is not computed.
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
P       (input) INTEGER
The number of rows of the matrix B.  P >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrices A and B.  N >= 0.
.TP 8
K       (input) INTEGER
L       (input) INTEGER
K and L specify the subblocks in the input matrices A and B:
.br
A23 = A(K+1:MIN(K+L,M),N-L+1:N) and B13 = B(1:L,,N-L+1:N)
of A and B, whose GSVD is going to be computed by ZTGSJA.
See Further details.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, A(N-K+1:N,1:MIN(K+L,M) ) contains the triangular
matrix R or part of R.  See Purpose for details.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,N)
On entry, the P-by-N matrix B.
On exit, if necessary, B(M-K+1:L,N+M-K-L+1:N) contains
a part of R.  See Purpose for details.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,P).
.TP 8
TOLA    (input) DOUBLE PRECISION
TOLB    (input) DOUBLE PRECISION
TOLA and TOLB are the convergence criteria for the Jacobi-
Kogbetliantz iteration procedure. Generally, they are the
same as used in the preprocessing step, say
TOLA = MAX(M,N)*norm(A)*MAZHEPS,
TOLB = MAX(P,N)*norm(B)*MAZHEPS.
.TP 8
ALPHA   (output) DOUBLE PRECISION array, dimension (N)
BETA    (output) DOUBLE PRECISION array, dimension (N)
On exit, ALPHA and BETA contain the generalized singular
value pairs of A and B;
ALPHA(1:K) = 1,
.br
BETA(1:K)  = 0,
and if M-K-L >= 0,
ALPHA(K+1:K+L) = diag(C),
.br
BETA(K+1:K+L)  = diag(S),
or if M-K-L < 0,
ALPHA(K+1:M)= C, ALPHA(M+1:K+L)= 0
.br
BETA(K+1:M) = S, BETA(M+1:K+L) = 1.
Furthermore, if K+L < N,
ALPHA(K+L+1:N) = 0
.br
BETA(K+L+1:N)  = 0.
.TP 8
U       (input/output) COMPLEX*16 array, dimension (LDU,M)
On entry, if JOBU = \(aqU\(aq, U must contain a matrix U1 (usually
the unitary matrix returned by ZGGSVP).
On exit,
if JOBU = \(aqI\(aq, U contains the unitary matrix U;
if JOBU = \(aqU\(aq, U contains the product U1*U.
If JOBU = \(aqN\(aq, U is not referenced.
.TP 8
LDU     (input) INTEGER
The leading dimension of the array U. LDU >= max(1,M) if
JOBU = \(aqU\(aq; LDU >= 1 otherwise.
.TP 8
V       (input/output) COMPLEX*16 array, dimension (LDV,P)
On entry, if JOBV = \(aqV\(aq, V must contain a matrix V1 (usually
the unitary matrix returned by ZGGSVP).
On exit,
if JOBV = \(aqI\(aq, V contains the unitary matrix V;
if JOBV = \(aqV\(aq, V contains the product V1*V.
If JOBV = \(aqN\(aq, V is not referenced.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V. LDV >= max(1,P) if
JOBV = \(aqV\(aq; LDV >= 1 otherwise.
.TP 8
Q       (input/output) COMPLEX*16 array, dimension (LDQ,N)
On entry, if JOBQ = \(aqQ\(aq, Q must contain a matrix Q1 (usually
the unitary matrix returned by ZGGSVP).
On exit,
if JOBQ = \(aqI\(aq, Q contains the unitary matrix Q;
if JOBQ = \(aqQ\(aq, Q contains the product Q1*Q.
If JOBQ = \(aqN\(aq, Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= max(1,N) if
JOBQ = \(aqQ\(aq; LDQ >= 1 otherwise.
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (2*N)
.TP 8
NCYCLE  (output) INTEGER
The number of cycles required for convergence.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
= 1:  the procedure does not converge after MAXIT cycles.
.SH PARAMETERS
.TP 8
MAXIT   INTEGER
MAXIT specifies the total loops that the iterative procedure
may take. If after MAXIT cycles, the routine fails to
converge, we return INFO = 1.

Further Details
===============

ZTGSJA essentially uses a variant of Kogbetliantz algorithm to reduce
min(L,M-K)-by-L triangular (or trapezoidal) matrix A23 and L-by-L
matrix B13 to the form:

U1\(aq*A13*Q1 = C1*R1; V1\(aq*B13*Q1 = S1*R1,

where U1, V1 and Q1 are unitary matrix, and Z\(aq is the conjugate
transpose of Z.  C1 and S1 are diagonal matrices satisfying

C1**2 + S1**2 = I,

and R1 is an L-by-L nonsingular upper triangular matrix.
