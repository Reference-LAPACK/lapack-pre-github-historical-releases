.TH  SLAQPS 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAQPS - a step of QR factorization with column pivoting of a real M-by-N matrix A by using Blas-3
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAQPS(
M, N, OFFSET, NB, KB, A, LDA, JPVT, TAU, VN1,
VN2, AUXV, F, LDF )
.TP 19
.ti +4
INTEGER
KB, LDA, LDF, M, N, NB, OFFSET
.TP 19
.ti +4
INTEGER
JPVT( * )
.TP 19
.ti +4
REAL
A( LDA, * ), AUXV( * ), F( LDF, * ), TAU( * ),
VN1( * ), VN2( * )
.SH PURPOSE
SLAQPS computes a step of QR factorization with column pivoting
of a real M-by-N matrix A by using Blas-3.  It tries to factorize
NB columns from A starting from the row OFFSET+1, and updates all
of the matrix with Blas-3 xGEMM.
.br

In some cases, due to catastrophic cancellations, it cannot
factorize NB columns.  Hence, the actual number of factorized
columns is returned in KB.
.br

Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A. N >= 0
.TP 8
OFFSET  (input) INTEGER
The number of rows of A that have been factorized in
previous steps.
.TP 8
NB      (input) INTEGER
The number of columns to factorize.
.TP 8
KB      (output) INTEGER
The number of columns actually factorized.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, block A(OFFSET+1:M,1:KB) is the triangular
.br
factor obtained and block A(1:OFFSET,1:N) has been
accordingly pivoted, but no factorized.
The rest of the matrix, block A(OFFSET+1:M,KB+1:N) has
been updated.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
JPVT    (input/output) INTEGER array, dimension (N)
JPVT(I) = K <==> Column K of the full matrix A has been
permuted into position I in AP.
.TP 8
TAU     (output) REAL array, dimension (KB)
The scalar factors of the elementary reflectors.
.TP 8
VN1     (input/output) REAL array, dimension (N)
The vector with the partial column norms.
.TP 8
VN2     (input/output) REAL array, dimension (N)
The vector with the exact column norms.
.TP 8
AUXV    (input/output) REAL array, dimension (NB)
Auxiliar vector.
.TP 8
F       (input/output) REAL array, dimension (LDF,NB)
Matrix F\(aq = L*Y\(aq*A.
.TP 8
LDF     (input) INTEGER
The leading dimension of the array F. LDF >= max(1,N).
.SH FURTHER DETAILS
Based on contributions by
.br
  G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
  X. Sun, Computer Science Dept., Duke University, USA
.br

Partial column norm updating strategy modified by
.br
  Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
.br
  University of Zagreb, Croatia.
.br
  June 2006.
.br
For more details see LAPACK Working Note 176.
.br
