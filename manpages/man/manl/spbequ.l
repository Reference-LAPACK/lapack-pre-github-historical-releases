.TH  SPBEQU 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPBEQU - row and column scalings intended to equilibrate a symmetric positive definite band matrix A and reduce its condition number (with respect to the two-norm)
.SH SYNOPSIS
.TP 19
SUBROUTINE SPBEQU(
UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, INFO )
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
AMAX, SCOND
.TP 19
.ti +4
REAL
AB( LDAB, * ), S( * )
.SH PURPOSE
SPBEQU computes row and column scalings intended to equilibrate a
symmetric positive definite band matrix A and reduce its condition
number (with respect to the two-norm).  S contains the scale factors,
S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
choice of S puts the condition number of B within a factor N of the
smallest possible condition number over all possible diagonal
scalings.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangular of A is stored;
.br
= \(aqL\(aq:  Lower triangular of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input) REAL array, dimension (LDAB,N)
The upper or lower triangle of the symmetric band matrix A,
stored in the first KD+1 rows of the array.  The j-th column
of A is stored in the j-th column of the array AB as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
.TP 9
LDAB     (input) INTEGER
The leading dimension of the array A.  LDAB >= KD+1.
.TP 8
S       (output) REAL array, dimension (N)
If INFO = 0, S contains the scale factors for A.
.TP 8
SCOND   (output) REAL
If INFO = 0, S contains the ratio of the smallest S(i) to
the largest S(i).  If SCOND >= 0.1 and AMAX is neither too
large nor too small, it is not worth scaling by S.
.TP 8
AMAX    (output) REAL
Absolute value of largest matrix element.  If AMAX is very
close to overflow or very close to underflow, the matrix
should be scaled.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = i, the i-th diagonal element is nonpositive.
