.TH  CHPTRI 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CHPTRI - the inverse of a complex Hermitian indefinite matrix A in packed storage using the factorization A = U*D*U**H or A = L*D*L**H computed by CHPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CHPTRI(
UPLO, N, AP, IPIV, WORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
COMPLEX
AP( * ), WORK( * )
.SH PURPOSE
CHPTRI computes the inverse of a complex Hermitian indefinite matrix
A in packed storage using the factorization A = U*D*U**H or
A = L*D*L**H computed by CHPTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the details of the factorization are stored
as an upper or lower triangular matrix.
= \(aqU\(aq:  Upper triangular, form is A = U*D*U**H;
.br
= \(aqL\(aq:  Lower triangular, form is A = L*D*L**H.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
AP      (input/output) COMPLEX array, dimension (N*(N+1)/2)
On entry, the block diagonal matrix D and the multipliers
used to obtain the factor U or L as computed by CHPTRF,
stored as a packed triangular matrix.

On exit, if INFO = 0, the (Hermitian) inverse of the original
matrix, stored as a packed triangular matrix. The j-th column
of inv(A) is stored in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = inv(A)(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq,
AP(i + (j-1)*(2n-j)/2) = inv(A)(i,j) for j<=i<=n.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D
as determined by CHPTRF.
.TP 8
WORK    (workspace) COMPLEX array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = i, D(i,i) = 0; the matrix is singular and its
inverse could not be computed.
