.TH  SSYGS2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SSYGS2 - a real symmetric-definite generalized eigenproblem to standard form
.SH SYNOPSIS
.TP 19
SUBROUTINE SSYGS2(
ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, ITYPE, LDA, LDB, N
.TP 19
.ti +4
REAL
A( LDA, * ), B( LDB, * )
.SH PURPOSE
SSYGS2 reduces a real symmetric-definite generalized eigenproblem
to standard form.

If ITYPE = 1, the problem is A*x = lambda*B*x,
.br
and A is overwritten by inv(U\(aq)*A*inv(U) or inv(L)*A*inv(L\(aq)

If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
.br
B*A*x = lambda*x, and A is overwritten by U*A*U` or L\(aq*A*L.

B must have been previously factorized as U\(aq*U or L*L\(aq by SPOTRF.

.SH ARGUMENTS
.TP 8
ITYPE   (input) INTEGER
= 1: compute inv(U\(aq)*A*inv(U) or inv(L)*A*inv(L\(aq);
.br
= 2 or 3: compute U*A*U\(aq or L\(aq*A*L.
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is stored, and how B has been factorized.
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the leading
n by n upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading n by n lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.

On exit, if INFO = 0, the transformed matrix, stored in the
same format as A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input) REAL array, dimension (LDB,N)
The triangular factor from the Cholesky factorization of B,
as returned by SPOTRF.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
