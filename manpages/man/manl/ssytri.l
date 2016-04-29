.TH  SSYTRI 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SSYTRI - the inverse of a real symmetric indefinite matrix A using the factorization A = U*D*U**T or A = L*D*L**T computed by SSYTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE SSYTRI(
UPLO, N, A, LDA, IPIV, WORK, INFO )
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
INTEGER
IPIV( * )
.TP 19
.ti +4
REAL
A( LDA, * ), WORK( * )
.SH PURPOSE
SSYTRI computes the inverse of a real symmetric indefinite matrix
A using the factorization A = U*D*U**T or A = L*D*L**T computed by
SSYTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the details of the factorization are stored
as an upper or lower triangular matrix.
= \(aqU\(aq:  Upper triangular, form is A = U*D*U**T;
.br
= \(aqL\(aq:  Lower triangular, form is A = L*D*L**T.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the block diagonal matrix D and the multipliers
used to obtain the factor U or L as computed by SSYTRF.

On exit, if INFO = 0, the (symmetric) inverse of the original
matrix.  If UPLO = \(aqU\(aq, the upper triangular part of the
inverse is formed and the part of A below the diagonal is not
referenced; if UPLO = \(aqL\(aq the lower triangular part of the
inverse is formed and the part of A above the diagonal is
not referenced.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (input) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D
as determined by SSYTRF.
.TP 8
WORK    (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = i, D(i,i) = 0; the matrix is singular and its
inverse could not be computed.
