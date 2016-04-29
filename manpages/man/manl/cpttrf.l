.TH  CPTTRF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CPTTRF - the L*D*L\(aq factorization of a complex Hermitian positive definite tridiagonal matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CPTTRF(
N, D, E, INFO )
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
REAL
D( * )
.TP 19
.ti +4
COMPLEX
E( * )
.SH PURPOSE
CPTTRF computes the L*D*L\(aq factorization of a complex Hermitian
positive definite tridiagonal matrix A.  The factorization may also
be regarded as having the form A = U\(aq*D*U.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the n diagonal elements of the tridiagonal matrix
A.  On exit, the n diagonal elements of the diagonal matrix
D from the L*D*L\(aq factorization of A.
.TP 8
E       (input/output) COMPLEX array, dimension (N-1)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix A.  On exit, the (n-1) subdiagonal elements of the
unit bidiagonal factor L from the L*D*L\(aq factorization of A.
E can also be regarded as the superdiagonal of the unit
bidiagonal factor U from the U\(aq*D*U factorization of A.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
.br
> 0: if INFO = k, the leading minor of order k is not
positive definite; if k < N, the factorization could not
be completed, while if k = N, the factorization was
completed, but D(N) <= 0.
