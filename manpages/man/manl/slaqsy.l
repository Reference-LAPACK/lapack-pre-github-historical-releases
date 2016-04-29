.TH  SLAQSY 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAQSY - a symmetric matrix A using the scaling factors in the vector S
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAQSY(
UPLO, N, A, LDA, S, SCOND, AMAX, EQUED )
.TP 19
.ti +4
CHARACTER
EQUED, UPLO
.TP 19
.ti +4
INTEGER
LDA, N
.TP 19
.ti +4
REAL
AMAX, SCOND
.TP 19
.ti +4
REAL
A( LDA, * ), S( * )
.SH PURPOSE
SLAQSY equilibrates a symmetric matrix A using the scaling factors
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
A       (input/output) REAL array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the leading
n by n upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading n by n lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.

On exit, if EQUED = \(aqY\(aq, the equilibrated matrix:
diag(S) * A * diag(S).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(N,1).
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
