.TH  CPPTRF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CPPTRF - the Cholesky factorization of a complex Hermitian positive definite matrix A stored in packed format
.SH SYNOPSIS
.TP 19
SUBROUTINE CPPTRF(
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
COMPLEX
AP( * )
.SH PURPOSE
CPPTRF computes the Cholesky factorization of a complex Hermitian
positive definite matrix A stored in packed format.

The factorization has the form
.br
   A = U**H * U,  if UPLO = \(aqU\(aq, or
.br
   A = L  * L**H,  if UPLO = \(aqL\(aq,
.br
where U is an upper triangular matrix and L is lower triangular.

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
AP      (input/output) COMPLEX array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the Hermitian matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
See below for further details.

On exit, if INFO = 0, the triangular factor U or L from the
Cholesky factorization A = U**H*U or A = L*L**H, in the same
storage format as A.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the leading minor of order i is not
positive definite, and the factorization could not be
completed.
.SH FURTHER DETAILS
The packed storage scheme is illustrated by the following example
when N = 4, UPLO = \(aqU\(aq:
.br

Two-dimensional storage of the Hermitian matrix A:
.br

   a11 a12 a13 a14
.br
       a22 a23 a24
.br
           a33 a34     (aij = conjg(aji))
.br
               a44
.br

Packed storage of the upper triangle of A:
.br

AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]

