.TH  CSPSV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CSPSV - the solution to a complex system of linear equations  A * X = B,
.SH SYNOPSIS
.TP 18
SUBROUTINE CSPSV(
UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
.TP 18
.ti +4
CHARACTER
UPLO
.TP 18
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 18
.ti +4
INTEGER
IPIV( * )
.TP 18
.ti +4
COMPLEX
AP( * ), B( LDB, * )
.SH PURPOSE
CSPSV computes the solution to a complex system of linear equations
   A * X = B,
where A is an N-by-N symmetric matrix stored in packed format and X
and B are N-by-NRHS matrices.
.br

The diagonal pivoting method is used to factor A as
.br
   A = U * D * U**T,  if UPLO = \(aqU\(aq, or
.br
   A = L * D * L**T,  if UPLO = \(aqL\(aq,
.br
where U (or L) is a product of permutation and unit upper (lower)
triangular matrices, D is symmetric and block diagonal with 1-by-1
and 2-by-2 diagonal blocks.  The factored form of A is then used to
solve the system of equations A * X = B.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The number of linear equations, i.e., the order of the
matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
AP      (input/output) COMPLEX array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the symmetric matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
See below for further details.

On exit, the block diagonal matrix D and the multipliers used
to obtain the factor U or L from the factorization
A = U*D*U**T or A = L*D*L**T as computed by CSPTRF, stored as
a packed triangular matrix in the same storage format as A.
.TP 8
IPIV    (output) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D, as
determined by CSPTRF.  If IPIV(k) > 0, then rows and columns
k and IPIV(k) were interchanged, and D(k,k) is a 1-by-1
diagonal block.  If UPLO = \(aqU\(aq and IPIV(k) = IPIV(k-1) < 0,
then rows and columns k-1 and -IPIV(k) were interchanged and
D(k-1:k,k-1:k) is a 2-by-2 diagonal block.  If UPLO = \(aqL\(aq and
IPIV(k) = IPIV(k+1) < 0, then rows and columns k+1 and
-IPIV(k) were interchanged and D(k:k+1,k:k+1) is a 2-by-2
diagonal block.
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
On entry, the N-by-NRHS right hand side matrix B.
On exit, if INFO = 0, the N-by-NRHS solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, D(i,i) is exactly zero.  The factorization
has been completed, but the block diagonal matrix D is
exactly singular, so the solution could not be
computed.
.SH FURTHER DETAILS
The packed storage scheme is illustrated by the following example
when N = 4, UPLO = \(aqU\(aq:
.br

Two-dimensional storage of the symmetric matrix A:
.br

   a11 a12 a13 a14
.br
       a22 a23 a24
.br
           a33 a34     (aij = aji)
.br
               a44
.br

Packed storage of the upper triangle of A:
.br

AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]

