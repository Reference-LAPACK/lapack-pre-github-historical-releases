.TH  ZHETRF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZHETRF - the factorization of a complex Hermitian matrix A using the Bunch-Kaufman diagonal pivoting method
.SH SYNOPSIS
.TP 19
SUBROUTINE ZHETRF(
UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, N
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), WORK( * )
.SH PURPOSE
ZHETRF computes the factorization of a complex Hermitian matrix A
using the Bunch-Kaufman diagonal pivoting method.  The form of the
factorization is
.br

   A = U*D*U**H  or  A = L*D*L**H
.br

where U (or L) is a product of permutation and unit upper (lower)
triangular matrices, and D is Hermitian and block diagonal with
1-by-1 and 2-by-2 diagonal blocks.
.br

This is the blocked version of the algorithm, calling Level 3 BLAS.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the Hermitian matrix A.  If UPLO = \(aqU\(aq, the leading
N-by-N upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading N-by-N lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.

On exit, the block diagonal matrix D and the multipliers used
to obtain the factor U or L (see below for further details).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (output) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D.
If IPIV(k) > 0, then rows and columns k and IPIV(k) were
interchanged and D(k,k) is a 1-by-1 diagonal block.
If UPLO = \(aqU\(aq and IPIV(k) = IPIV(k-1) < 0, then rows and
columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
is a 2-by-2 diagonal block.  If UPLO = \(aqL\(aq and IPIV(k) =
IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were
interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of WORK.  LWORK >=1.  For best performance
LWORK >= N*NB, where NB is the block size returned by ILAENV.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, D(i,i) is exactly zero.  The factorization
has been completed, but the block diagonal matrix D is
exactly singular, and division by zero will occur if it
is used to solve a system of equations.
.SH FURTHER DETAILS
If UPLO = \(aqU\(aq, then A = U*D*U\(aq, where
.br
   U = P(n)*U(n)* ... *P(k)U(k)* ...,
.br
i.e., U is a product of terms P(k)*U(k), where k decreases from n to
1 in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1
and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as
defined by IPIV(k), and U(k) is a unit upper triangular matrix, such
that if the diagonal block D(k) is of order s (s = 1 or 2), then

           (   I    v    0   )   k-s
.br
   U(k) =  (   0    I    0   )   s
.br
           (   0    0    I   )   n-k
.br
              k-s   s   n-k
.br

If s = 1, D(k) overwrites A(k,k), and v overwrites A(1:k-1,k).
If s = 2, the upper triangle of D(k) overwrites A(k-1,k-1), A(k-1,k),
and A(k,k), and v overwrites A(1:k-2,k-1:k).
.br

If UPLO = \(aqL\(aq, then A = L*D*L\(aq, where
.br
   L = P(1)*L(1)* ... *P(k)*L(k)* ...,
.br
i.e., L is a product of terms P(k)*L(k), where k increases from 1 to
n in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1
and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as
defined by IPIV(k), and L(k) is a unit lower triangular matrix, such
that if the diagonal block D(k) is of order s (s = 1 or 2), then

           (   I    0     0   )  k-1
.br
   L(k) =  (   0    I     0   )  s
.br
           (   0    v     I   )  n-k-s+1
.br
              k-1   s  n-k-s+1
.br

If s = 1, D(k) overwrites A(k,k), and v overwrites A(k+1:n,k).
If s = 2, the lower triangle of D(k) overwrites A(k,k), A(k+1,k),
and A(k+1,k+1), and v overwrites A(k+2:n,k:k+1).
.br

