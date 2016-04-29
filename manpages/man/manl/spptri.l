.TH  SPPTRI 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPPTRI - the inverse of a real symmetric positive definite matrix A using the Cholesky factorization A = U**T*U or A = L*L**T computed by SPPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE SPPTRI(
UPLO, N, AP, INFO )
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
REAL
AP( * )
.SH PURPOSE
SPPTRI computes the inverse of a real symmetric positive definite
matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
computed by SPPTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangular factor is stored in AP;
.br
= \(aqL\(aq:  Lower triangular factor is stored in AP.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
AP      (input/output) REAL array, dimension (N*(N+1)/2)
On entry, the triangular factor U or L from the Cholesky
factorization A = U**T*U or A = L*L**T, packed columnwise as
a linear array.  The j-th column of U or L is stored in the
array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = U(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = L(i,j) for j<=i<=n.

On exit, the upper or lower triangle of the (symmetric)
inverse of A, overwriting the input factor U or L.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the (i,i) element of the factor U or L is
zero, and the inverse could not be computed.
