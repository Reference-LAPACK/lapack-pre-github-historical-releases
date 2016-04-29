.TH  DSYTF2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DSYTF2 - the factorization of a real symmetric matrix A using the Bunch-Kaufman diagonal pivoting method
.SH SYNOPSIS
.TP 19
SUBROUTINE DSYTF2(
UPLO, N, A, LDA, IPIV, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * )
.SH PURPOSE
DSYTF2 computes the factorization of a real symmetric matrix A using
the Bunch-Kaufman diagonal pivoting method:

   A = U*D*U\(aq  or  A = L*D*L\(aq
.br

where U (or L) is a product of permutation and unit upper (lower)
triangular matrices, U\(aq is the transpose of U, and D is symmetric and
block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
.br

This is the unblocked version of the algorithm, calling Level 2 BLAS.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is stored:
.br
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the leading
n-by-n upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading n-by-n lower triangular part of A contains the lower
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
INFO    (output) INTEGER
.br
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
.br
> 0: if INFO = k, D(k,k) is exactly zero.  The factorization
has been completed, but the block diagonal matrix D is
exactly singular, and division by zero will occur if it
is used to solve a system of equations.
.SH FURTHER DETAILS
09-29-06 - patch from
.br
  Bobby Cheng, MathWorks
.br

  Replace l.204 and l.372
.br
       IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN
.br
  by
.br
       IF( (MAX( ABSAKK, COLMAX ).EQ.ZERO) .OR. DISNAN(ABSAKK) ) THEN

01-01-96 - Based on modifications by
.br
  J. Lewis, Boeing Computer Services Company
.br
  A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
1-96 - Based on modifications by J. Lewis, Boeing Computer Services
       Company
.br

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

