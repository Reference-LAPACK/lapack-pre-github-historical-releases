.TH  ZGGSVD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZGGSVD - the generalized singular value decomposition (GSVD) of an M-by-N complex matrix A and P-by-N complex matrix B
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGGSVD(
JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,
LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ, WORK,
RWORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBQ, JOBU, JOBV
.TP 19
.ti +4
INTEGER
INFO, K, L, LDA, LDB, LDQ, LDU, LDV, M, N, P
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION ALPHA( * ), BETA( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
U( LDU, * ), V( LDV, * ), WORK( * )
.SH PURPOSE
ZGGSVD computes the generalized singular value decomposition (GSVD)
of an M-by-N complex matrix A and P-by-N complex matrix B:

      U\(aq*A*Q = D1*( 0 R ),    V\(aq*B*Q = D2*( 0 R )
.br

where U, V and Q are unitary matrices, and Z\(aq means the conjugate
transpose of Z.  Let K+L = the effective numerical rank of the
matrix (A\(aq,B\(aq)\(aq, then R is a (K+L)-by-(K+L) nonsingular upper
triangular matrix, D1 and D2 are M-by-(K+L) and P-by-(K+L) "diagonal"
matrices and of the following structures, respectively:
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
       D2 =   L ( 0  S )
.br
            P-L ( 0  0 )
.br

                N-K-L  K    L
.br
  ( 0 R ) = K (  0   R11  R12 )
.br
            L (  0    0   R22 )
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
       D2 =   M-K ( 0  S    0  )
.br
            K+L-M ( 0  0    I  )
.br
              P-L ( 0  0    0  )
.br

                   N-K-L  K   M-K  K+L-M
.br
  ( 0 R ) =     K ( 0    R11  R12  R13  )
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

  (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
  ( 0  R22 R23 )
.br
  in B(M-K+1:L,N+M-K-L+1:N) on exit.
.br

The routine computes C, S, R, and optionally the unitary
.br
transformation matrices U, V and Q.
.br

In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
A and B implicitly gives the SVD of A*inv(B):
.br
                     A*inv(B) = U*(D1*inv(D2))*V\(aq.
.br
If ( A\(aq,B\(aq)\(aq has orthnormal columns, then the GSVD of A and B is also
equal to the CS decomposition of A and B. Furthermore, the GSVD can
be used to derive the solution of the eigenvalue problem:
                     A\(aq*A x = lambda* B\(aq*B x.
.br
In some literature, the GSVD of A and B is presented in the form
                 U\(aq*A*X = ( 0 D1 ),   V\(aq*B*X = ( 0 D2 )
.br
where U and V are orthogonal and X is nonsingular, and D1 and D2 are
``diagonal\(aq\(aq.  The former GSVD form can be converted to the latter
form by taking the nonsingular matrix X as
.br

                      X = Q*(  I   0    )
.br
                            (  0 inv(R) )
.br

.SH ARGUMENTS
.TP 8
JOBU    (input) CHARACTER*1
= \(aqU\(aq:  Unitary matrix U is computed;
.br
= \(aqN\(aq:  U is not computed.
.TP 8
JOBV    (input) CHARACTER*1
.br
= \(aqV\(aq:  Unitary matrix V is computed;
.br
= \(aqN\(aq:  V is not computed.
.TP 8
JOBQ    (input) CHARACTER*1
.br
= \(aqQ\(aq:  Unitary matrix Q is computed;
.br
= \(aqN\(aq:  Q is not computed.
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrices A and B.  N >= 0.
.TP 8
P       (input) INTEGER
The number of rows of the matrix B.  P >= 0.
.TP 8
K       (output) INTEGER
L       (output) INTEGER
On exit, K and L specify the dimension of the subblocks
described in Purpose.
K + L = effective numerical rank of (A\(aq,B\(aq)\(aq.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, A contains the triangular matrix R, or part of R.
See Purpose for details.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,N)
On entry, the P-by-N matrix B.
On exit, B contains part of the triangular matrix R if
M-K-L < 0.  See Purpose for details.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,P).
.TP 8
ALPHA   (output) DOUBLE PRECISION array, dimension (N)
BETA    (output) DOUBLE PRECISION array, dimension (N)
On exit, ALPHA and BETA contain the generalized singular
value pairs of A and B;
ALPHA(1:K) = 1,
.br
BETA(1:K)  = 0,
and if M-K-L >= 0,
ALPHA(K+1:K+L) = C,
.br
BETA(K+1:K+L)  = S,
or if M-K-L < 0,
ALPHA(K+1:M)= C, ALPHA(M+1:K+L)= 0
.br
BETA(K+1:M) = S, BETA(M+1:K+L) = 1
and
ALPHA(K+L+1:N) = 0
.br
BETA(K+L+1:N)  = 0
.TP 8
U       (output) COMPLEX*16 array, dimension (LDU,M)
If JOBU = \(aqU\(aq, U contains the M-by-M unitary matrix U.
If JOBU = \(aqN\(aq, U is not referenced.
.TP 8
LDU     (input) INTEGER
The leading dimension of the array U. LDU >= max(1,M) if
JOBU = \(aqU\(aq; LDU >= 1 otherwise.
.TP 8
V       (output) COMPLEX*16 array, dimension (LDV,P)
If JOBV = \(aqV\(aq, V contains the P-by-P unitary matrix V.
If JOBV = \(aqN\(aq, V is not referenced.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V. LDV >= max(1,P) if
JOBV = \(aqV\(aq; LDV >= 1 otherwise.
.TP 8
Q       (output) COMPLEX*16 array, dimension (LDQ,N)
If JOBQ = \(aqQ\(aq, Q contains the N-by-N unitary matrix Q.
If JOBQ = \(aqN\(aq, Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= max(1,N) if
JOBQ = \(aqQ\(aq; LDQ >= 1 otherwise.
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (max(3*N,M,P)+N)
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (N)
On exit, IWORK stores the sorting information. More
precisely, the following loop will sort ALPHA
for I = K+1, min(M,K+L)
swap ALPHA(I) and ALPHA(IWORK(I))
endfor
such that ALPHA(1) >= ALPHA(2) >= ... >= ALPHA(N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = 1, the Jacobi-type procedure failed to
converge.  For further details, see subroutine ZTGSJA.
.SH PARAMETERS
.TP 8
TOLA    DOUBLE PRECISION
TOLB    DOUBLE PRECISION
TOLA and TOLB are the thresholds to determine the effective
rank of (A\(aq,B\(aq)\(aq. Generally, they are set to
TOLA = MAX(M,N)*norm(A)*MAZHEPS,
TOLB = MAX(P,N)*norm(B)*MAZHEPS.
The size of TOLA and TOLB may affect the size of backward
errors of the decomposition.

Further Details
===============

2-96 Based on modifications by
Ming Gu and Huan Ren, Computer Science Division, University of
California at Berkeley, USA
