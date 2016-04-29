.TH  SPBTF2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPBTF2 - the Cholesky factorization of a real symmetric positive definite band matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE SPBTF2(
UPLO, N, KD, AB, LDAB, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, N
.TP 19
.ti +4
REAL
AB( LDAB, * )
.SH PURPOSE
SPBTF2 computes the Cholesky factorization of a real symmetric
positive definite band matrix A.

The factorization has the form
.br
   A = U\(aq * U ,  if UPLO = \(aqU\(aq, or
.br
   A = L  * L\(aq,  if UPLO = \(aqL\(aq,
.br
where U is an upper triangular matrix, U\(aq is the transpose of U, and
L is lower triangular.
.br

This is the unblocked version of the algorithm, calling Level 2 BLAS.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is stored:
.br
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of super-diagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of sub-diagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input/output) REAL array, dimension (LDAB,N)
On entry, the upper or lower triangle of the symmetric band
matrix A, stored in the first KD+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).

On exit, if INFO = 0, the triangular factor U or L from the
Cholesky factorization A = U\(aq*U or A = L*L\(aq of the band
matrix A, in the same storage format as A.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
.br
> 0: if INFO = k, the leading minor of order k is not
positive definite, and the factorization could not be
completed.
.SH FURTHER DETAILS
The band storage scheme is illustrated by the following example, when
N = 6, KD = 2, and UPLO = \(aqU\(aq:
.br

On entry:                       On exit:
.br

    *    *   a13  a24  a35  a46      *    *   u13  u24  u35  u46
    *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
   a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66

Similarly, if UPLO = \(aqL\(aq the format of A is as follows:
.br

On entry:                       On exit:
.br

   a11  a22  a33  a44  a55  a66     l11  l22  l33  l44  l55  l66
   a21  a32  a43  a54  a65   *      l21  l32  l43  l54  l65   *
   a31  a42  a53  a64   *    *      l31  l42  l53  l64   *    *

Array elements marked * are not used by the routine.
.br

