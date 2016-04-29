.TH  SLAQSP 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAQSP - a symmetric matrix A using the scaling factors in the vector S
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAQSP(
UPLO, N, AP, S, SCOND, AMAX, EQUED )
.TP 19
.ti +4
CHARACTER
EQUED, UPLO
.TP 19
.ti +4
INTEGER
N
.TP 19
.ti +4
REAL
AMAX, SCOND
.TP 19
.ti +4
REAL
AP( * ), S( * )
.SH PURPOSE
SLAQSP equilibrates a symmetric matrix A using the scaling factors
in the vector S.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is stored.
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
AP      (input/output) REAL array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the symmetric matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.

On exit, the equilibrated matrix:  diag(S) * A * diag(S), in
the same storage format as A.
.TP 8
S       (input) REAL array, dimension (N)
The scale factors for A.
.TP 8
SCOND   (input) REAL
Ratio of the smallest S(i) to the largest S(i).
.TP 8
AMAX    (input) REAL
Absolute value of largest matrix entry.
.TP 8
EQUED   (output) CHARACTER*1
Specifies whether or not equilibration was done.
= \(aqN\(aq:  No equilibration.
.br
= \(aqY\(aq:  Equilibration was done, i.e., A has been replaced by
diag(S) * A * diag(S).
.SH PARAMETERS

THRESH is a threshold value used to decide if scaling should be done
based on the ratio of the scaling factors.  If SCOND < THRESH,
scaling is done.

LARGE and SMALL are threshold values used to decide if scaling should
be done based on the absolute size of the largest matrix element.
If AMAX > LARGE or AMAX < SMALL, scaling is done.
