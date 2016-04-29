.TH  CLACP2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLACP2 - all or part of a real two-dimensional matrix A to a complex matrix B
.SH SYNOPSIS
.TP 19
SUBROUTINE CLACP2(
UPLO, M, N, A, LDA, B, LDB )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
LDA, LDB, M, N
.TP 19
.ti +4
REAL
A( LDA, * )
.TP 19
.ti +4
COMPLEX
B( LDB, * )
.SH PURPOSE
CLACP2 copies all or part of a real two-dimensional matrix A to a
complex matrix B.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies the part of the matrix A to be copied to B.
= \(aqU\(aq:      Upper triangular part
.br
= \(aqL\(aq:      Lower triangular part
.br
Otherwise:  All of the matrix A
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input) REAL array, dimension (LDA,N)
The m by n matrix A.  If UPLO = \(aqU\(aq, only the upper trapezium
is accessed; if UPLO = \(aqL\(aq, only the lower trapezium is
accessed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
B       (output) COMPLEX array, dimension (LDB,N)
On exit, B = A in the locations specified by UPLO.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,M).
