.TH  ZHPGST 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZHPGST - a complex Hermitian-definite generalized eigenproblem to standard form, using packed storage
.SH SYNOPSIS
.TP 19
SUBROUTINE ZHPGST(
ITYPE, UPLO, N, AP, BP, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, ITYPE, N
.TP 19
.ti +4
COMPLEX*16
AP( * ), BP( * )
.SH PURPOSE
ZHPGST reduces a complex Hermitian-definite generalized
eigenproblem to standard form, using packed storage.

If ITYPE = 1, the problem is A*x = lambda*B*x,
.br
and A is overwritten by inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H)

If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
.br
B*A*x = lambda*x, and A is overwritten by U*A*U**H or L**H*A*L.

B must have been previously factorized as U**H*U or L*L**H by ZPPTRF.

.SH ARGUMENTS
.TP 8
ITYPE   (input) INTEGER
= 1: compute inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H);
.br
= 2 or 3: compute U*A*U**H or L**H*A*L.
.TP 8
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq:  Upper triangle of A is stored and B is factored as
U**H*U;
= \(aqL\(aq:  Lower triangle of A is stored and B is factored as
L*L**H.
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
AP      (input/output) COMPLEX*16 array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the Hermitian matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.

On exit, if INFO = 0, the transformed matrix, stored in the
same format as A.
.TP 8
BP      (input) COMPLEX*16 array, dimension (N*(N+1)/2)
The triangular factor from the Cholesky factorization of B,
stored in the same format as A, as returned by ZPPTRF.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
