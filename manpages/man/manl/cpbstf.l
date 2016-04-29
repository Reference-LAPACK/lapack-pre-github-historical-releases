.TH  CPBSTF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CPBSTF - a split Cholesky factorization of a complex Hermitian positive definite band matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CPBSTF(
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
COMPLEX
AB( LDAB, * )
.SH PURPOSE
CPBSTF computes a split Cholesky factorization of a complex
Hermitian positive definite band matrix A.

This routine is designed to be used in conjunction with CHBGST.

The factorization has the form  A = S**H*S  where S is a band matrix
of the same bandwidth as A and the following structure:
.br

  S = ( U    )
.br
      ( M  L )
.br

where U is upper triangular of order m = (n+kd)/2, and L is lower
triangular of order n-m.
.br

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
KD      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input/output) COMPLEX array, dimension (LDAB,N)
On entry, the upper or lower triangle of the Hermitian band
matrix A, stored in the first kd+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).

On exit, if INFO = 0, the factor S from the split Cholesky
factorization A = S**H*S. See Further Details.
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = i, the factorization could not be completed,
because the updated element a(i,i) was negative; the
matrix A is not positive definite.
.SH FURTHER DETAILS
The band storage scheme is illustrated by the following example, when
N = 7, KD = 2:
.br

S = ( s11  s12  s13                     )
.br
    (      s22  s23  s24                )
.br
    (           s33  s34                )
.br
    (                s44                )
.br
    (           s53  s54  s55           )
.br
    (                s64  s65  s66      )
.br
    (                     s75  s76  s77 )
.br

If UPLO = \(aqU\(aq, the array AB holds:
.br

on entry:                          on exit:
.br

 *    *   a13  a24  a35  a46  a57   *    *   s13  s24  s53\(aq s64\(aq s75\(aq
 *   a12  a23  a34  a45  a56  a67   *   s12  s23  s34  s54\(aq s65\(aq s76\(aq
a11  a22  a33  a44  a55  a66  a77  s11  s22  s33  s44  s55  s66  s77

If UPLO = \(aqL\(aq, the array AB holds:
.br

on entry:                          on exit:
.br

a11  a22  a33  a44  a55  a66  a77  s11  s22  s33  s44  s55  s66  s77
a21  a32  a43  a54  a65  a76   *   s12\(aq s23\(aq s34\(aq s54  s65  s76   *
a31  a42  a53  a64  a64   *    *   s13\(aq s24\(aq s53  s64  s75   *    *

Array elements marked * are not used by the routine; s12\(aq denotes
conjg(s12); the diagonal elements of S are real.
.br

