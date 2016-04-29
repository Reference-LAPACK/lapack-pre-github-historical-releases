.TH  DPORFS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DPORFS - the computed solution to a system of linear equations when the coefficient matrix is symmetric positive definite,
.SH SYNOPSIS
.TP 19
SUBROUTINE DPORFS(
UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,
LDX, FERR, BERR, WORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LDAF, LDB, LDX, N, NRHS
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
BERR( * ), FERR( * ), WORK( * ), X( LDX, * )
.SH PURPOSE
DPORFS improves the computed solution to a system of linear
equations when the coefficient matrix is symmetric positive definite,
and provides error bounds and backward error estimates for the
solution.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices B and X.  NRHS >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,N)
The symmetric matrix A.  If UPLO = \(aqU\(aq, the leading N-by-N
upper triangular part of A contains the upper triangular part
of the matrix A, and the strictly lower triangular part of A
is not referenced.  If UPLO = \(aqL\(aq, the leading N-by-N lower
triangular part of A contains the lower triangular part of
the matrix A, and the strictly upper triangular part of A is
not referenced.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
AF      (input) DOUBLE PRECISION array, dimension (LDAF,N)
The triangular factor U or L from the Cholesky factorization
A = U**T*U or A = L*L**T, as computed by DPOTRF.
.TP 8
LDAF    (input) INTEGER
The leading dimension of the array AF.  LDAF >= max(1,N).
.TP 8
B       (input) DOUBLE PRECISION array, dimension (LDB,NRHS)
The right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (input/output) DOUBLE PRECISION array, dimension (LDX,NRHS)
On entry, the solution matrix X, as computed by DPOTRS.
On exit, the improved solution matrix X.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
The estimated forward error bound for each solution vector
X(j) (the j-th column of the solution matrix X).
If XTRUE is the true solution corresponding to X(j), FERR(j)
is an estimated upper bound for the magnitude of the largest
element in (X(j) - XTRUE) divided by the magnitude of the
largest element in X(j).  The estimate is as reliable as
the estimate for RCOND, and is almost always a slight
overestimate of the true error.
.TP 8
BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
The componentwise relative backward error of each solution
vector X(j) (i.e., the smallest relative change in
any element of A or B that makes X(j) an exact solution).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH PARAMETERS

ITMAX is the maximum number of steps of iterative refinement.
