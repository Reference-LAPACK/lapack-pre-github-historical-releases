.TH  CTBTRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CTBTRS - a triangular system of the form   A * X = B, A**T * X = B, or A**H * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE CTBTRS(
UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,
LDB, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, LDB, N, NRHS
.TP 19
.ti +4
COMPLEX
AB( LDAB, * ), B( LDB, * )
.SH PURPOSE
CTBTRS solves a triangular system of the form

where A is a triangular band matrix of order N, and B is an
N-by-NRHS matrix.  A check is made to verify that A is nonsingular.

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
of the matrix B.  NRHS >= 0.
.TP 8
AB      (input) COMPLEX array, dimension (LDAB,N)
The upper or lower triangular band matrix A, stored in the
first kd+1 rows of AB.  The j-th column of A is stored
in the j-th column of the array AB as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
If DIAG = \(aqU\(aq, the diagonal elements of A are not referenced
and are assumed to be 1.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit, if INFO = 0, the solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the i-th diagonal element of A is zero,
indicating that the matrix is singular and the
solutions X have not been computed.
