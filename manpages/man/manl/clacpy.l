.TH  CLACPY 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLACPY - all or part of a two-dimensional matrix A to another matrix B
.SH SYNOPSIS
.TP 19
SUBROUTINE CLACPY(
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
COMPLEX
A( LDA, * ), B( LDB, * )
.SH PURPOSE
CLACPY copies all or part of a two-dimensional matrix A to another
matrix B.

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
A       (input) COMPLEX array, dimension (LDA,N)
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
