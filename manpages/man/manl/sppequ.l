.TH  SPPEQU 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SPPEQU - row and column scalings intended to equilibrate a symmetric positive definite matrix A in packed storage and reduce its condition number (with respect to the two-norm)
.SH SYNOPSIS
.TP 19
SUBROUTINE SPPEQU(
UPLO, N, AP, S, SCOND, AMAX, INFO )
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
AMAX, SCOND
.TP 19
.ti +4
REAL
AP( * ), S( * )
.SH PURPOSE
SPPEQU computes row and column scalings intended to equilibrate a
symmetric positive definite matrix A in packed storage and reduce
its condition number (with respect to the two-norm).  S contains the
scale factors, S(i)=1/sqrt(A(i,i)), chosen so that the scaled matrix
B with elements B(i,j)=S(i)*A(i,j)*S(j) has ones on the diagonal.
This choice of S puts the condition number of B within a factor N of
the smallest possible condition number over all possible diagonal
scalings.
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
AP      (input) REAL array, dimension (N*(N+1)/2)
The upper or lower triangle of the symmetric matrix A, packed
columnwise in a linear array.  The j-th column of A is stored
in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
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
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the i-th diagonal element is nonpositive.
