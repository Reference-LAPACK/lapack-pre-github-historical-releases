.TH  SPPRFS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPPRFS - the computed solution to a system of linear equations when the coefficient matrix is symmetric positive definite and packed, and provides error bounds and backward error estimates for the solution
.SH SYNOPSIS
.TP 19
SUBROUTINE SPPRFS(
UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR,
BERR, WORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDB, LDX, N, NRHS
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
AFP( * ), AP( * ), B( LDB, * ), BERR( * ),
FERR( * ), WORK( * ), X( LDX, * )
.SH PURPOSE
SPPRFS improves the computed solution to a system of linear
equations when the coefficient matrix is symmetric positive definite
and packed, and provides error bounds and backward error estimates
for the solution.

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
AP      (input) REAL array, dimension (N*(N+1)/2)
The upper or lower triangle of the symmetric matrix A, packed
columnwise in a linear array.  The j-th column of A is stored
in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
.TP 8
AFP     (input) REAL array, dimension (N*(N+1)/2)
The triangular factor U or L from the Cholesky factorization
A = U**T*U or A = L*L**T, as computed by SPPTRF/CPPTRF,
packed columnwise in a linear array in the same format as A
(see AP).
.TP 8
B       (input) REAL array, dimension (LDB,NRHS)
The right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (input/output) REAL array, dimension (LDX,NRHS)
On entry, the solution matrix X, as computed by SPPTRS.
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
WORK    (workspace) REAL array, dimension (3*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH PARAMETERS

ITMAX is the maximum number of steps of iterative refinement.
