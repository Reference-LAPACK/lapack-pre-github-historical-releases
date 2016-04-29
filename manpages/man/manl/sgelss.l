.TH  SGELSS 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
SGELSS - the minimum norm solution to a real linear least squares problem
.SH SYNOPSIS
.TP 19
SUBROUTINE SGELSS(
M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
.TP 19
.ti +4
REAL
RCOND
.TP 19
.ti +4
REAL
A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
.SH PURPOSE
SGELSS computes the minimum norm solution to a real linear least
squares problem:

Minimize 2-norm(| b - A*x |).
.br

using the singular value decomposition (SVD) of A. A is an M-by-N
matrix which may be rank-deficient.
.br

Several right hand side vectors b and solution vectors x can be
handled in a single call; they are stored as the columns of the
M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
X.
.br

The effective rank of A is determined by treating as zero those
singular values which are less than RCOND times the largest singular
value.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A. N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices B and X. NRHS >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, the first min(m,n) rows of A are overwritten with
its right singular vectors, stored rowwise.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
B       (input/output) REAL array, dimension (LDB,NRHS)
On entry, the M-by-NRHS right hand side matrix B.
On exit, B is overwritten by the N-by-NRHS solution
matrix X.  If m >= n and RANK = n, the residual
sum-of-squares for the solution in the i-th column is given
by the sum of squares of elements n+1:m in that column.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,max(M,N)).
.TP 8
S       (output) REAL array, dimension (min(M,N))
The singular values of A in decreasing order.
The condition number of A in the 2-norm = S(1)/S(min(m,n)).
.TP 8
RCOND   (input) REAL
RCOND is used to determine the effective rank of A.
Singular values S(i) <= RCOND*S(1) are treated as zero.
If RCOND < 0, machine precision is used instead.
.TP 8
RANK    (output) INTEGER
The effective rank of A, i.e., the number of singular values
which are greater than RCOND*S(1).
.TP 8
WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= 1, and also:
LWORK >= 3*min(M,N) + max( 2*min(M,N), max(M,N), NRHS )
For good performance, LWORK should generally be larger.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  the algorithm for computing the SVD failed to converge;
if INFO = i, i off-diagonal elements of an intermediate
bidiagonal form did not converge to zero.
