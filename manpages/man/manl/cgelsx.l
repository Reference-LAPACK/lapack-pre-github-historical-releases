.TH  CGELSX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CGELSX - i deprecated and has been replaced by routine CGELSY
.SH SYNOPSIS
.TP 19
SUBROUTINE CGELSX(
M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
WORK, RWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, M, N, NRHS, RANK
.TP 19
.ti +4
REAL
RCOND
.TP 19
.ti +4
INTEGER
JPVT( * )
.TP 19
.ti +4
REAL
RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), WORK( * )
.SH PURPOSE
This routine is deprecated and has been replaced by routine CGELSY.

CGELSX computes the minimum-norm solution to a complex linear least
squares problem:
.br
    minimize || A * X - B ||
.br
using a complete orthogonal factorization of A.  A is an M-by-N
matrix which may be rank-deficient.
.br

Several right hand side vectors b and solution vectors x can be
handled in a single call; they are stored as the columns of the
M-by-NRHS right hand side matrix B and the N-by-NRHS solution
matrix X.
.br

The routine first computes a QR factorization with column pivoting:
    A * P = Q * [ R11 R12 ]
.br
                [  0  R22 ]
.br
with R11 defined as the largest leading submatrix whose estimated
condition number is less than 1/RCOND.  The order of R11, RANK,
is the effective rank of A.
.br

Then, R22 is considered to be negligible, and R12 is annihilated
by unitary transformations from the right, arriving at the
complete orthogonal factorization:
.br
   A * P = Q * [ T11 0 ] * Z
.br
               [  0  0 ]
.br
The minimum-norm solution is then
.br
   X = P * Z\(aq [ inv(T11)*Q1\(aq*B ]
.br
              [        0       ]
.br
where Q1 consists of the first RANK columns of Q.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of
columns of matrices B and X. NRHS >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, A has been overwritten by details of its
complete orthogonal factorization.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
On entry, the M-by-NRHS right hand side matrix B.
On exit, the N-by-NRHS solution matrix X.
If m >= n and RANK = n, the residual sum-of-squares for
the solution in the i-th column is given by the sum of
squares of elements N+1:M in that column.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,M,N).
.TP 8
JPVT    (input/output) INTEGER array, dimension (N)
On entry, if JPVT(i) .ne. 0, the i-th column of A is an
initial column, otherwise it is a free column.  Before
the QR factorization of A, all initial columns are
permuted to the leading positions; only the remaining
free columns are moved as a result of column pivoting
during the factorization.
On exit, if JPVT(i) = k, then the i-th column of A*P
was the k-th column of A.
.TP 8
RCOND   (input) REAL
RCOND is used to determine the effective rank of A, which
is defined as the order of the largest leading triangular
submatrix R11 in the QR factorization with pivoting of A,
whose estimated condition number < 1/RCOND.
.TP 8
RANK    (output) INTEGER
The effective rank of A, i.e., the order of the submatrix
R11.  This is the same as the order of the submatrix T11
in the complete orthogonal factorization of A.
.TP 8
WORK    (workspace) COMPLEX array, dimension
(min(M,N) + max( N, 2*min(M,N)+NRHS )),
.TP 8
RWORK   (workspace) REAL array, dimension (2*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
