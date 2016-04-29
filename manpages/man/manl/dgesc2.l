.TH  DGESC2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DGESC2 - a system of linear equations   A * X = scale* RHS  with a general N-by-N matrix A using the LU factorization with complete pivoting computed by DGETC2
.SH SYNOPSIS
.TP 19
SUBROUTINE DGESC2(
N, A, LDA, RHS, IPIV, JPIV, SCALE )
.TP 19
.ti +4
INTEGER
LDA, N
.TP 19
.ti +4
DOUBLE
PRECISION SCALE
.TP 19
.ti +4
INTEGER
IPIV( * ), JPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), RHS( * )
.SH PURPOSE
DGESC2 solves a system of linear equations

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the  LU part of the factorization of the n-by-n
matrix A computed by DGETC2:  A = P * L * U * Q
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1, N).
.TP 8
RHS     (input/output) DOUBLE PRECISION array, dimension (N).
On entry, the right hand side vector b.
On exit, the solution vector X.
.TP 8
IPIV    (input) INTEGER array, dimension (N).
The pivot indices; for 1 <= i <= N, row i of the
matrix has been interchanged with row IPIV(i).
.TP 8
JPIV    (input) INTEGER array, dimension (N).
The pivot indices; for 1 <= j <= N, column j of the
matrix has been interchanged with column JPIV(j).
.TP 9
SCALE    (output) DOUBLE PRECISION
On exit, SCALE contains the scale factor. SCALE is chosen
0 <= SCALE <= 1 to prevent owerflow in the solution.
.SH FURTHER DETAILS
Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
.br

