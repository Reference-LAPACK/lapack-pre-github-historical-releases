.TH  CLAQHB 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLAQHB - an Hermitian band matrix A using the scaling factors in the vector S
.SH SYNOPSIS
.TP 19
SUBROUTINE CLAQHB(
UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, EQUED )
.TP 19
.ti +4
CHARACTER
EQUED, UPLO
.TP 19
.ti +4
INTEGER
KD, LDAB, N
.TP 19
.ti +4
REAL
AMAX, SCOND
.TP 19
.ti +4
REAL
S( * )
.TP 19
.ti +4
COMPLEX
AB( LDAB, * )
.SH PURPOSE
CLAQHB equilibrates an Hermitian band matrix A using the scaling
factors in the vector S.

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
KD      (input) INTEGER
The number of super-diagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of sub-diagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input/output) COMPLEX array, dimension (LDAB,N)
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
S       (output) REAL array, dimension (N)
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
