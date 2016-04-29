.TH  CPTRFS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CPTRFS - the computed solution to a system of linear equations when the coefficient matrix is Hermitian positive definite and tridiagonal, and provides error bounds and backward error estimates for the solution
.SH SYNOPSIS
.TP 19
SUBROUTINE CPTRFS(
UPLO, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,
FERR, BERR, WORK, RWORK, INFO )
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
REAL
BERR( * ), D( * ), DF( * ), FERR( * ),
RWORK( * )
.TP 19
.ti +4
COMPLEX
B( LDB, * ), E( * ), EF( * ), WORK( * ),
X( LDX, * )
.SH PURPOSE
CPTRFS improves the computed solution to a system of linear
equations when the coefficient matrix is Hermitian positive definite
and tridiagonal, and provides error bounds and backward error
estimates for the solution.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the superdiagonal or the subdiagonal of the
tridiagonal matrix A is stored and the form of the
factorization:
.br
= \(aqU\(aq:  E is the superdiagonal of A, and A = U**H*D*U;
.br
= \(aqL\(aq:  E is the subdiagonal of A, and A = L*D*L**H.
(The two forms are equivalent if A is real.)
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
D       (input) REAL array, dimension (N)
The n real diagonal elements of the tridiagonal matrix A.
.TP 8
E       (input) COMPLEX array, dimension (N-1)
The (n-1) off-diagonal elements of the tridiagonal matrix A
(see UPLO).
.TP 8
DF      (input) REAL array, dimension (N)
The n diagonal elements of the diagonal matrix D from
the factorization computed by CPTTRF.
.TP 8
EF      (input) COMPLEX array, dimension (N-1)
The (n-1) off-diagonal elements of the unit bidiagonal
factor U or L from the factorization computed by CPTTRF
(see UPLO).
.TP 8
B       (input) COMPLEX array, dimension (LDB,NRHS)
The right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (input/output) COMPLEX array, dimension (LDX,NRHS)
On entry, the solution matrix X, as computed by CPTTRS.
On exit, the improved solution matrix X.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
FERR    (output) REAL array, dimension (NRHS)
The forward error bound for each solution vector
X(j) (the j-th column of the solution matrix X).
If XTRUE is the true solution corresponding to X(j), FERR(j)
is an estimated upper bound for the magnitude of the largest
element in (X(j) - XTRUE) divided by the magnitude of the
largest element in X(j).
.TP 8
BERR    (output) REAL array, dimension (NRHS)
The componentwise relative backward error of each solution
vector X(j) (i.e., the smallest relative change in
any element of A or B that makes X(j) an exact solution).
.TP 8
WORK    (workspace) COMPLEX array, dimension (N)
.TP 8
RWORK   (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH PARAMETERS

ITMAX is the maximum number of steps of iterative refinement.
