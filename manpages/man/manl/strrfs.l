.TH  STRRFS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
STRRFS - error bounds and backward error estimates for the solution to a system of linear equations with a triangular coefficient matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE STRRFS(
UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,
LDX, FERR, BERR, WORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LDX, N, NRHS
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
A( LDA, * ), B( LDB, * ), BERR( * ), FERR( * ),
WORK( * ), X( LDX, * )
.SH PURPOSE
STRRFS provides error bounds and backward error estimates for the
solution to a system of linear equations with a triangular
coefficient matrix.

The solution matrix X must be computed by STRTRS or some other
means before entering this routine.  STRRFS does not do iterative
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
= \(aqN\(aq:  A * X = B  (No transpose)
.br
= \(aqT\(aq:  A**T * X = B  (Transpose)
.br
= \(aqC\(aq:  A**H * X = B  (Conjugate transpose = Transpose)
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
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices B and X.  NRHS >= 0.
.TP 8
A       (input) REAL array, dimension (LDA,N)
The triangular matrix A.  If UPLO = \(aqU\(aq, the leading N-by-N
upper triangular part of the array A contains the upper
triangular matrix, and the strictly lower triangular part of
A is not referenced.  If UPLO = \(aqL\(aq, the leading N-by-N lower
triangular part of the array A contains the lower triangular
matrix, and the strictly upper triangular part of A is not
referenced.  If DIAG = \(aqU\(aq, the diagonal elements of A are
also not referenced and are assumed to be 1.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input) REAL array, dimension (LDB,NRHS)
The right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (input) REAL array, dimension (LDX,NRHS)
The solution matrix X.
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
