.TH  ZLACRM 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLACRM - a very simple matrix-matrix multiplication
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLACRM(
M, N, A, LDA, B, LDB, C, LDC, RWORK )
.TP 19
.ti +4
INTEGER
LDA, LDB, LDC, M, N
.TP 19
.ti +4
DOUBLE
PRECISION B( LDB, * ), RWORK( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), C( LDC, * )
.SH PURPOSE
ZLACRM performs a very simple matrix-matrix multiplication:
         C := A * B,
.br
where A is M by N and complex; B is N by N and real;
.br
C is M by N and complex.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A and of the matrix C.
M >= 0.
.TP 8
N       (input) INTEGER
The number of columns and rows of the matrix B and
the number of columns of the matrix C.
N >= 0.
.TP 8
A       (input) COMPLEX*16 array, dimension (LDA, N)
A contains the M by N matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >=max(1,M).
.TP 8
B       (input) DOUBLE PRECISION array, dimension (LDB, N)
B contains the N by N matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >=max(1,N).
.TP 8
C       (input) COMPLEX*16 array, dimension (LDC, N)
C contains the M by N matrix C.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >=max(1,N).
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (2*M*N)
