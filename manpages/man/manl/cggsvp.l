.SH NAME
CGGSVP - unitary matrices U, V and Q such that   N-K-L K L  U\(aq*A*Q = K ( 0 A12 A13 ) if M-K-L >= 0
.SH SYNOPSIS
.TP 19
SUBROUTINE CGGSVP(
JOBU, JOBV, JOBQ, M, P, N, A, LDA, B, LDB,
TOLA, TOLB, K, L, U, LDU, V, LDV, Q, LDQ,
IWORK, RWORK, TAU, WORK, INFO )
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
REAL
TOLA, TOLB
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
TAU( * ), U( LDU, * ), V( LDV, * ), WORK( * )
.SH PURPOSE
CGGSVP computes unitary matrices U, V and Q such that
              L ( 0     0   A23 )
.br
          M-K-L ( 0     0    0  )
.br

                 N-K-L  K    L
.br
        =     K ( 0    A12  A13 )  if M-K-L < 0;
.br
            M-K ( 0     0   A23 )
.br

               N-K-L  K    L
.br
 V\(aq*B*Q =   L ( 0     0   B13 )
.br
          P-L ( 0     0    0  )
.br

where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0,
otherwise A23 is (M-K)-by-L upper trapezoidal.  K+L = the effective
numerical rank of the (M+P)-by-N matrix (A\(aq,B\(aq)\(aq.  Z\(aq denotes the
conjugate transpose of Z.
.br

This decomposition is the preprocessing step for computing the
Generalized Singular Value Decomposition (GSVD), see subroutine
CGGSVD.
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
P       (input) INTEGER
The number of rows of the matrix B.  P >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrices A and B.  N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, A contains the triangular (or trapezoidal) matrix
described in the Purpose section.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,N)
On entry, the P-by-N matrix B.
On exit, B contains the triangular matrix described in
the Purpose section.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,P).
.TP 8
TOLA    (input) REAL
TOLB    (input) REAL
TOLA and TOLB are the thresholds to determine the effective
numerical rank of matrix B and a subblock of A. Generally,
they are set to
TOLA = MAX(M,N)*norm(A)*MACHEPS,
TOLB = MAX(P,N)*norm(B)*MACHEPS.
The size of TOLA and TOLB may affect the size of backward
errors of the decomposition.
.TP 8
K       (output) INTEGER
L       (output) INTEGER
On exit, K and L specify the dimension of the subblocks
described in Purpose section.
K + L = effective numerical rank of (A\(aq,B\(aq)\(aq.
.TP 8
U       (output) COMPLEX array, dimension (LDU,M)
If JOBU = \(aqU\(aq, U contains the unitary matrix U.
If JOBU = \(aqN\(aq, U is not referenced.
.TP 8
LDU     (input) INTEGER
The leading dimension of the array U. LDU >= max(1,M) if
JOBU = \(aqU\(aq; LDU >= 1 otherwise.
.TP 8
V       (output) COMPLEX array, dimension (LDV,M)
If JOBV = \(aqV\(aq, V contains the unitary matrix V.
If JOBV = \(aqN\(aq, V is not referenced.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V. LDV >= max(1,P) if
JOBV = \(aqV\(aq; LDV >= 1 otherwise.
.TP 8
Q       (output) COMPLEX array, dimension (LDQ,N)
If JOBQ = \(aqQ\(aq, Q contains the unitary matrix Q.
If JOBQ = \(aqN\(aq, Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q. LDQ >= max(1,N) if
JOBQ = \(aqQ\(aq; LDQ >= 1 otherwise.
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
RWORK   (workspace) REAL array, dimension (2*N)
.TP 8
TAU     (workspace) COMPLEX array, dimension (N)
.TP 8
WORK    (workspace) COMPLEX array, dimension (max(3*N,M,P))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
The subroutine uses LAPACK subroutine CGEQPF for the QR factorization
with column pivoting to detect the effective numerical rank of the
a matrix. It may be replaced by a better rank determination strategy.

