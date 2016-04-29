.TH  CGELS 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CGELS - overdetermined or underdetermined complex linear systems involving an M-by-N matrix A, or its conjugate-transpose, using a QR or LQ factorization of A
.SH SYNOPSIS
.TP 18
SUBROUTINE CGELS(
TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,
INFO )
.TP 18
.ti +4
CHARACTER
TRANS
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, M, N, NRHS
.TP 18
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), WORK( * )
.SH PURPOSE
CGELS solves overdetermined or underdetermined complex linear systems
involving an M-by-N matrix A, or its conjugate-transpose, using a QR
or LQ factorization of A.  It is assumed that A has full rank.

The following options are provided:
.br

1. If TRANS = \(aqN\(aq and m >= n:  find the least squares solution of
   an overdetermined system, i.e., solve the least squares problem
                minimize || B - A*X ||.
.br

2. If TRANS = \(aqN\(aq and m < n:  find the minimum norm solution of
   an underdetermined system A * X = B.
.br

3. If TRANS = \(aqC\(aq and m >= n:  find the minimum norm solution of
   an undetermined system A**H * X = B.
.br

4. If TRANS = \(aqC\(aq and m < n:  find the least squares solution of
   an overdetermined system, i.e., solve the least squares problem
                minimize || B - A**H * X ||.
.br

Several right hand side vectors b and solution vectors x can be
handled in a single call; they are stored as the columns of the
M-by-NRHS right hand side matrix B and the N-by-NRHS solution
matrix X.
.br

.SH ARGUMENTS
.TP 8
TRANS   (input) CHARACTER*1
= \(aqN\(aq: the linear system involves A;
.br
= \(aqC\(aq: the linear system involves A**H.
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of
columns of the matrices B and X. NRHS >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the M-by-N matrix A.
if M >= N, A is overwritten by details of its QR
factorization as returned by CGEQRF;
if M <  N, A is overwritten by details of its LQ
factorization as returned by CGELQF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
On entry, the matrix B of right hand side vectors, stored
columnwise; B is M-by-NRHS if TRANS = \(aqN\(aq, or N-by-NRHS
if TRANS = \(aqC\(aq.
On exit, if INFO = 0, B is overwritten by the solution
vectors, stored columnwise:
if TRANS = \(aqN\(aq and m >= n, rows 1 to n of B contain the least
squares solution vectors; the residual sum of squares for the
solution in each column is given by the sum of squares of the
modulus of elements N+1 to M in that column;
if TRANS = \(aqN\(aq and m < n, rows 1 to N of B contain the
minimum norm solution vectors;
if TRANS = \(aqC\(aq and m >= n, rows 1 to M of B contain the
minimum norm solution vectors;
if TRANS = \(aqC\(aq and m < n, rows 1 to M of B contain the
least squares solution vectors; the residual sum of squares
for the solution in each column is given by the sum of
squares of the modulus of elements M+1 to N in that column.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= MAX(1,M,N).
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
LWORK >= max( 1, MN + max( MN, NRHS ) ).
For optimal performance,
LWORK >= max( 1, MN + max( MN, NRHS )*NB ).
where MN = min(M,N) and NB is the optimum block size.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO =  i, the i-th diagonal element of the
triangular factor of A is zero, so that A does not have
full rank; the least squares solution could not be
computed.
