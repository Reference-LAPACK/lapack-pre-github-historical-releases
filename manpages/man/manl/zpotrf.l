.TH  ZPOTRF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZPOTRF - the Cholesky factorization of a complex Hermitian positive definite matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE ZPOTRF(
UPLO, N, A, LDA, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * )
.SH PURPOSE
ZPOTRF computes the Cholesky factorization of a complex Hermitian
positive definite matrix A.

The factorization has the form
.br
   A = U**H * U,  if UPLO = \(aqU\(aq, or
.br
   A = L  * L**H,  if UPLO = \(aqL\(aq,
.br
where U is an upper triangular matrix and L is lower triangular.

This is the block version of the algorithm, calling Level 3 BLAS.

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
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the leading minor of order i is not
positive definite, and the factorization could not be
completed.
