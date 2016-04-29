.TH  ZPOSV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZPOSV - the solution to a complex system of linear equations  A * X = B,
.SH SYNOPSIS
.TP 18
SUBROUTINE ZPOSV(
UPLO, N, NRHS, A, LDA, B, LDB, INFO )
.TP 18
.ti +4
CHARACTER
UPLO
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, N, NRHS
.TP 18
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * )
.SH PURPOSE
ZPOSV computes the solution to a complex system of linear equations
   A * X = B,
where A is an N-by-N Hermitian positive definite matrix and X and B
are N-by-NRHS matrices.
.br

The Cholesky decomposition is used to factor A as
.br
   A = U**H* U,  if UPLO = \(aqU\(aq, or
.br
   A = L * L**H,  if UPLO = \(aqL\(aq,
.br
where U is an upper triangular matrix and  L is a lower triangular
matrix.  The factored form of A is then used to solve the system of
equations A * X = B.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The number of linear equations, i.e., the order of the
matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the Hermitian matrix A.  If UPLO = \(aqU\(aq, the leading
N-by-N upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading N-by-N lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.

On exit, if INFO = 0, the factor U or L from the Cholesky
factorization A = U**H*U or A = L*L**H.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
On entry, the N-by-NRHS right hand side matrix B.
On exit, if INFO = 0, the N-by-NRHS solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the leading minor of order i of A is not
positive definite, so the factorization could not be
completed, and the solution has not been computed.
