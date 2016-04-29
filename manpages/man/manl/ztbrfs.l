.TH  ZTBRFS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTBRFS - error bounds and backward error estimates for the solution to a system of linear equations with a triangular band coefficient matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTBRFS(
UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,
LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, LDB, LDX, N, NRHS
.TP 19
.ti +4
DOUBLE
PRECISION BERR( * ), FERR( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX*16
AB( LDAB, * ), B( LDB, * ), WORK( * ),
X( LDX, * )
.SH PURPOSE
ZTBRFS provides error bounds and backward error estimates for the
solution to a system of linear equations with a triangular band
coefficient matrix.

The solution matrix X must be computed by ZTBTRS or some other
means before entering this routine.  ZTBRFS does not do iterative
refinement because doing so cannot improve the backward error.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  A is upper triangular;
.br
= \(aqL\(aq:  A is lower triangular.
.TP 8
TRANS   (input) CHARACTER*1
.br
Specifies the form of the system of equations:
.br
= \(aqN\(aq:  A * X = B     (No transpose)
.br
= \(aqT\(aq:  A**T * X = B  (Transpose)
.br
= \(aqC\(aq:  A**H * X = B  (Conjugate transpose)
.TP 8
DIAG    (input) CHARACTER*1
.br
= \(aqN\(aq:  A is non-unit triangular;
.br
= \(aqU\(aq:  A is unit triangular.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of superdiagonals or subdiagonals of the
triangular band matrix A.  KD >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices B and X.  NRHS >= 0.
.TP 8
AB      (input) COMPLEX*16 array, dimension (LDAB,N)
The upper or lower triangular band matrix A, stored in the
first kd+1 rows of the array. The j-th column of A is stored
in the j-th column of the array AB as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
If DIAG = \(aqU\(aq, the diagonal elements of A are not referenced
and are assumed to be 1.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
B       (input) COMPLEX*16 array, dimension (LDB,NRHS)
The right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (input) COMPLEX*16 array, dimension (LDX,NRHS)
The solution matrix X.
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
WORK    (workspace) COMPLEX*16 array, dimension (2*N)
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
