.TH  ZGELSD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZGELSD - the minimum-norm solution to a real linear least squares problem
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGELSD(
M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
WORK, LWORK, RWORK, IWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
.TP 19
.ti +4
DOUBLE
PRECISION RCOND
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * ), S( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), WORK( * )
.SH PURPOSE
ZGELSD computes the minimum-norm solution to a real linear least
squares problem:
    minimize 2-norm(| b - A*x |)
.br
using the singular value decomposition (SVD) of A. A is an M-by-N
matrix which may be rank-deficient.
.br

Several right hand side vectors b and solution vectors x can be
handled in a single call; they are stored as the columns of the
M-by-NRHS right hand side matrix B and the N-by-NRHS solution
matrix X.
.br

The problem is solved in three steps:
.br
(1) Reduce the coefficient matrix A to bidiagonal form with
    Householder tranformations, reducing the original problem
    into a "bidiagonal least squares problem" (BLS)
.br
(2) Solve the BLS using a divide and conquer approach.
.br
(3) Apply back all the Householder tranformations to solve
    the original least squares problem.
.br

The effective rank of A is determined by treating as zero those
singular values which are less than RCOND times the largest singular
value.
.br

The divide and conquer algorithm makes very mild assumptions about
floating point arithmetic. It will work on machines with a guard
digit in add/subtract, or on those binary machines without guard
digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
Cray-2. It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
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
A       (input) COMPLEX*16 array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, A has been destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
On entry, the M-by-NRHS right hand side matrix B.
On exit, B is overwritten by the N-by-NRHS solution matrix X.
If m >= n and RANK = n, the residual sum-of-squares for
the solution in the i-th column is given by the sum of
squares of the modulus of elements n+1:m in that column.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,M,N).
.TP 8
S       (output) DOUBLE PRECISION array, dimension (min(M,N))
The singular values of A in decreasing order.
The condition number of A in the 2-norm = S(1)/S(min(m,n)).
.TP 8
RCOND   (input) DOUBLE PRECISION
RCOND is used to determine the effective rank of A.
Singular values S(i) <= RCOND*S(1) are treated as zero.
If RCOND < 0, machine precision is used instead.
.TP 8
RANK    (output) INTEGER
The effective rank of A, i.e., the number of singular values
which are greater than RCOND*S(1).
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK must be at least 1.
The exact minimum amount of workspace needed depends on M,
N and NRHS. As long as LWORK is at least
2*N + N*NRHS
if M is greater than or equal to N or
2*M + M*NRHS
if M is less than N, the code will execute correctly.
For good performance, LWORK should generally be larger.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the array WORK and the
minimum sizes of the arrays RWORK and IWORK, and returns
these values as the first entries of the WORK, RWORK and
IWORK arrays, and no error message related to LWORK is issued
by XERBLA.
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (MAX(1,LRWORK))
LRWORK >=
10*N + 2*N*SMLSIZ + 8*N*NLVL + 3*SMLSIZ*NRHS +
(SMLSIZ+1)**2
if M is greater than or equal to N or
10*M + 2*M*SMLSIZ + 8*M*NLVL + 3*SMLSIZ*NRHS +
(SMLSIZ+1)**2
if M is less than N, the code will execute correctly.
SMLSIZ is returned by ILAENV and is equal to the maximum
size of the subproblems at the bottom of the computation
tree (usually about 25), and
NLVL = MAX( 0, INT( LOG_2( MIN( M,N )/(SMLSIZ+1) ) ) + 1 )
On exit, if INFO = 0, RWORK(1) returns the minimum LRWORK.
.TP 8
IWORK   (workspace) INTEGER array, dimension (MAX(1,LIWORK))
LIWORK >= max(1, 3*MINMN*NLVL + 11*MINMN),
where MINMN = MIN( M,N ).
On exit, if INFO = 0, IWORK(1) returns the minimum LIWORK.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  the algorithm for computing the SVD failed to converge;
if INFO = i, i off-diagonal elements of an intermediate
bidiagonal form did not converge to zero.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Ren-Cang Li, Computer Science Division, University of
     California at Berkeley, USA
.br
   Osni Marques, LBNL/NERSC, USA
.br

