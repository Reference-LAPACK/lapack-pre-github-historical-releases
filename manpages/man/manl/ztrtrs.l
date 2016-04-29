.TH  ZTRTRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTRTRS - a triangular system of the form   A * X = B, A**T * X = B, or A**H * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTRTRS(
UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,
INFO )
.TP 19
.ti +4
CHARACTER
DIAG, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, N, NRHS
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * )
.SH PURPOSE
ZTRTRS solves a triangular system of the form

where A is a triangular matrix of order N, and B is an N-by-NRHS
matrix.  A check is made to verify that A is nonsingular.

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
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
A       (input) COMPLEX*16 array, dimension (LDA,N)
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
B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit, if INFO = 0, the solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = i, the i-th diagonal element of A is zero,
indicating that the matrix is singular and the solutions
X have not been computed.
