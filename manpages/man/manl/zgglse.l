.TH  ZGGLSE 1 "February 2007" " LAPACK driver routine (version 3.1.1) " " LAPACK driver routine (version 3.1.1) " 
.SH NAME
ZGGLSE - the linear equality-constrained least squares (LSE) problem
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGGLSE(
M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,
INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, M, N, P
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), C( * ), D( * ),
WORK( * ), X( * )
.SH PURPOSE
ZGGLSE solves the linear equality-constrained least squares (LSE)
problem:

        minimize || c - A*x ||_2   subject to   B*x = d
.br

where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
M-vector, and d is a given P-vector. It is assumed that
.br
P <= N <= M+P, and
.br

         rank(B) = P and  rank( ( A ) ) = N.
.br
                              ( ( B ) )
.br

These conditions ensure that the LSE problem has a unique solution,
which is obtained using a generalized RQ factorization of the
matrices (B, A) given by
.br

   B = (0 R)*Q,   A = Z*T*Q.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrices A and B. N >= 0.
.TP 8
P       (input) INTEGER
The number of rows of the matrix B. 0 <= P <= N <= M+P.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, the elements on and above the diagonal of the array
contain the min(M,N)-by-N upper trapezoidal matrix T.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,N)
On entry, the P-by-N matrix B.
On exit, the upper triangle of the subarray B(1:P,N-P+1:N)
contains the P-by-P upper triangular matrix R.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,P).
.TP 8
C       (input/output) COMPLEX*16 array, dimension (M)
On entry, C contains the right hand side vector for the
least squares part of the LSE problem.
On exit, the residual sum of squares for the solution
is given by the sum of squares of elements N-P+1 to M of
vector C.
.TP 8
D       (input/output) COMPLEX*16 array, dimension (P)
On entry, D contains the right hand side vector for the
constrained equation.
On exit, D is destroyed.
.TP 8
X       (output) COMPLEX*16 array, dimension (N)
On exit, X is the solution of the LSE problem.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,M+N+P).
For optimum performance LWORK >= P+min(M,N)+max(M,N)*NB,
where NB is an upper bound for the optimal blocksizes for
ZGEQRF, CGERQF, ZUNMQR and CUNMRQ.

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
= 1:  the upper triangular factor R associated with B in the
generalized RQ factorization of the pair (B, A) is
singular, so that rank(B) < P; the least squares
solution could not be computed.
= 2:  the (N-P) by (N-P) part of the upper trapezoidal factor
T associated with A in the generalized RQ factorization
of the pair (B, A) is singular, so that
rank( (A) ) < N; the least squares solution could not
( (B) )
be computed.
