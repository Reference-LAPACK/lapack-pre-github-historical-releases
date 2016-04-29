.TH  SGETC2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SGETC2 - an LU factorization with complete pivoting of the n-by-n matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE SGETC2(
N, A, LDA, IPIV, JPIV, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
INTEGER
IPIV( * ), JPIV( * )
.TP 19
.ti +4
REAL
A( LDA, * )
.SH PURPOSE
SGETC2 computes an LU factorization with complete pivoting of the
n-by-n matrix A. The factorization has the form A = P * L * U * Q,
where P and Q are permutation matrices, L is lower triangular with
unit diagonal elements and U is upper triangular.
.br

This is the Level 2 BLAS algorithm.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A. N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA, N)
On entry, the n-by-n matrix A to be factored.
On exit, the factors L and U from the factorization
A = P*L*U*Q; the unit diagonal elements of L are not stored.
If U(k, k) appears to be less than SMIN, U(k, k) is given the
value of SMIN, i.e., giving a nonsingular perturbed system.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (output) INTEGER array, dimension(N).
The pivot indices; for 1 <= i <= N, row i of the
matrix has been interchanged with row IPIV(i).
.TP 8
JPIV    (output) INTEGER array, dimension(N).
The pivot indices; for 1 <= j <= N, column j of the
matrix has been interchanged with column JPIV(j).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
> 0: if INFO = k, U(k, k) is likely to produce owerflow if
we try to solve for x in Ax = b. So U is perturbed to
avoid the overflow.
.SH FURTHER DETAILS
Based on contributions by
.br
   Bo Kagstrom and Peter Poromaa, Department of Computing Science,
   Umea University, S-901 87 Umea, Sweden.
.br

