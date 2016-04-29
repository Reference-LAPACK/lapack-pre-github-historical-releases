.TH  CLASET 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLASET - a 2-D array A to BETA on the diagonal and ALPHA on the offdiagonals
.SH SYNOPSIS
.TP 19
SUBROUTINE CLASET(
UPLO, M, N, ALPHA, BETA, A, LDA )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
LDA, M, N
.TP 19
.ti +4
COMPLEX
ALPHA, BETA
.TP 19
.ti +4
COMPLEX
A( LDA, * )
.SH PURPOSE
CLASET initializes a 2-D array A to BETA on the diagonal and
ALPHA on the offdiagonals.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies the part of the matrix A to be set.
= \(aqU\(aq:      Upper triangular part is set. The lower triangle
is unchanged.
= \(aqL\(aq:      Lower triangular part is set. The upper triangle
is unchanged.
Otherwise:  All of the matrix A is set.
.TP 8
M       (input) INTEGER
On entry, M specifies the number of rows of A.
.TP 8
N       (input) INTEGER
On entry, N specifies the number of columns of A.
.TP 8
ALPHA   (input) COMPLEX
All the offdiagonal array elements are set to ALPHA.
.TP 8
BETA    (input) COMPLEX
All the diagonal array elements are set to BETA.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the m by n matrix A.
On exit, A(i,j) = ALPHA, 1 <= i <= m, 1 <= j <= n, i.ne.j;
A(i,i) = BETA , 1 <= i <= min(m,n)
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
