.TH  CGERFS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CGERFS - the computed solution to a system of linear equations and provides error bounds and backward error estimates for the solution
.SH SYNOPSIS
.TP 19
SUBROUTINE CGERFS(
TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,
X, LDX, FERR, BERR, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
TRANS
.TP 19
.ti +4
INTEGER
INFO, LDA, LDAF, LDB, LDX, N, NRHS
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
REAL
BERR( * ), FERR( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
WORK( * ), X( LDX, * )
.SH PURPOSE
CGERFS improves the computed solution to a system of linear
equations and provides error bounds and backward error estimates for
the solution.

.SH ARGUMENTS
.TP 8
TRANS   (input) CHARACTER*1
Specifies the form of the system of equations:
.br
= \(aqN\(aq:  A * X = B     (No transpose)
.br
= \(aqT\(aq:  A**T * X = B  (Transpose)
.br
= \(aqC\(aq:  A**H * X = B  (Conjugate transpose)
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices B and X.  NRHS >= 0.
.TP 8
A       (input) COMPLEX array, dimension (LDA,N)
The original N-by-N matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
AF      (input) COMPLEX array, dimension (LDAF,N)
The factors L and U from the factorization A = P*L*U
as computed by CGETRF.
.TP 8
LDAF    (input) INTEGER
The leading dimension of the array AF.  LDAF >= max(1,N).
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices from CGETRF; for 1<=i<=N, row i of the
matrix was interchanged with row IPIV(i).
.TP 8
B       (input) COMPLEX array, dimension (LDB,NRHS)
The right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (input/output) COMPLEX array, dimension (LDX,NRHS)
On entry, the solution matrix X, as computed by CGETRS.
On exit, the improved solution matrix X.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
FERR    (output) REAL array, dimension (NRHS)
The estimated forward error bound for each solution vector
X(j) (the j-th column of the solution matrix X).
If XTRUE is the true solution corresponding to X(j), FERR(j)
is an estimated upper bound for the magnitude of the largest
element in (X(j) - XTRUE) divided by the magnitude of the
largest element in X(j).  The estimate is as reliable as
the estimate for RCOND, and is almost always a slight
overestimate of the true error.
.TP 8
BERR    (output) REAL array, dimension (NRHS)
The componentwise relative backward error of each solution
vector X(j) (i.e., the smallest relative change in
any element of A or B that makes X(j) an exact solution).
.TP 8
WORK    (workspace) COMPLEX array, dimension (2*N)
.TP 8
RWORK   (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH PARAMETERS

ITMAX is the maximum number of steps of iterative refinement.
