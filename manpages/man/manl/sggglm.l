.TH  SGGGLM 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
SGGGLM - a general Gauss-Markov linear model (GLM) problem
.SH SYNOPSIS
.TP 19
SUBROUTINE SGGGLM(
N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK,
INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, M, N, P
.TP 19
.ti +4
REAL
A( LDA, * ), B( LDB, * ), D( * ), WORK( * ),
X( * ), Y( * )
.SH PURPOSE
SGGGLM solves a general Gauss-Markov linear model (GLM) problem:

        minimize || y ||_2   subject to   d = A*x + B*y
.br
            x
.br

where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
given N-vector. It is assumed that M <= N <= M+P, and
.br

           rank(A) = M    and    rank( A B ) = N.
.br

Under these assumptions, the constrained equation is always
consistent, and there is a unique solution x and a minimal 2-norm
solution y, which is obtained using a generalized QR factorization
of the matrices (A, B) given by
.br

   A = Q*(R),   B = Q*T*Z.
.br
         (0)
.br

In particular, if matrix B is square nonsingular, then the problem
GLM is equivalent to the following weighted linear least squares
problem
.br

             minimize || inv(B)*(d-A*x) ||_2
.br
                 x
.br

where inv(B) denotes the inverse of B.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of rows of the matrices A and B.  N >= 0.
.TP 8
M       (input) INTEGER
The number of columns of the matrix A.  0 <= M <= N.
.TP 8
P       (input) INTEGER
The number of columns of the matrix B.  P >= N-M.
.TP 8
A       (input/output) REAL array, dimension (LDA,M)
On entry, the N-by-M matrix A.
On exit, the upper triangular part of the array A contains
the M-by-M upper triangular matrix R.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
B       (input/output) REAL array, dimension (LDB,P)
On entry, the N-by-P matrix B.
On exit, if N <= P, the upper triangle of the subarray
B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
if N > P, the elements on and above the (N-P)th subdiagonal
contain the N-by-P upper trapezoidal matrix T.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, D is the left hand side of the GLM equation.
On exit, D is destroyed.
.TP 8
X       (output) REAL array, dimension (M)
Y       (output) REAL array, dimension (P)
On exit, X and Y are the solutions of the GLM problem.
.TP 8
WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,N+M+P).
For optimum performance, LWORK >= M+min(N,P)+max(N,P)*NB,
where NB is an upper bound for the optimal blocksizes for
SGEQRF, SGERQF, SORMQR and SORMRQ.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
= 1:  the upper triangular factor R associated with A in the
generalized QR factorization of the pair (A, B) is
singular, so that rank(A) < M; the least squares
solution could not be computed.
= 2:  the bottom (N-M) by (N-M) part of the upper trapezoidal
factor T associated with B in the generalized QR
factorization of the pair (A, B) is singular, so that
rank( A B ) < N; the least squares solution could not
be computed.
