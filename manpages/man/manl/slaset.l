.TH  SLASET 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLASET - an m-by-n matrix A to BETA on the diagonal and ALPHA on the offdiagonals
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASET(
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
REAL
ALPHA, BETA
.TP 19
.ti +4
REAL
A( LDA, * )
.SH PURPOSE
SLASET initializes an m-by-n matrix A to BETA on the diagonal and
ALPHA on the offdiagonals.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies the part of the matrix A to be set.
= \(aqU\(aq:      Upper triangular part is set; the strictly lower
triangular part of A is not changed.
= \(aqL\(aq:      Lower triangular part is set; the strictly upper
triangular part of A is not changed.
Otherwise:  All of the matrix A is set.
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
ALPHA   (input) REAL
The constant to which the offdiagonal elements are to be set.
.TP 8
BETA    (input) REAL
The constant to which the diagonal elements are to be set.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On exit, the leading m-by-n submatrix of A is set as follows:

if UPLO = \(aqU\(aq, A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
if UPLO = \(aqL\(aq, A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,

and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
