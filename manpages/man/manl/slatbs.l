.TH  SLATBS 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLATBS - one of the triangular systems   A *x = s*b or A\(aq*x = s*b  with scaling to prevent overflow, where A is an upper or lower triangular band matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE SLATBS(
UPLO, TRANS, DIAG, NORMIN, N, KD, AB, LDAB, X,
SCALE, CNORM, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, NORMIN, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, N
.TP 19
.ti +4
REAL
SCALE
.TP 19
.ti +4
REAL
AB( LDAB, * ), CNORM( * ), X( * )
.SH PURPOSE
SLATBS solves one of the triangular systems
are n-element vectors, and s is a scaling factor, usually less than
or equal to 1, chosen so that the components of x will be less than
the overflow threshold.  If the unscaled problem will not cause
overflow, the Level 2 BLAS routine STBSV is called.  If the matrix A
is singular (A(j,j) = 0 for some j), then s is set to 0 and a
non-trivial solution to A*x = 0 is returned.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the matrix A is upper or lower triangular.
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
TRANS   (input) CHARACTER*1
Specifies the operation applied to A.
= \(aqN\(aq:  Solve A * x = s*b  (No transpose)
.br
= \(aqT\(aq:  Solve A\(aq* x = s*b  (Transpose)
.br
= \(aqC\(aq:  Solve A\(aq* x = s*b  (Conjugate transpose = Transpose)
.TP 8
DIAG    (input) CHARACTER*1
Specifies whether or not the matrix A is unit triangular.
= \(aqN\(aq:  Non-unit triangular
.br
= \(aqU\(aq:  Unit triangular
.TP 8
NORMIN  (input) CHARACTER*1
Specifies whether CNORM has been set or not.
= \(aqY\(aq:  CNORM contains the column norms on entry
.br
= \(aqN\(aq:  CNORM is not set on entry.  On exit, the norms will
be computed and stored in CNORM.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of subdiagonals or superdiagonals in the
triangular matrix A.  KD >= 0.
.TP 8
AB      (input) REAL array, dimension (LDAB,N)
The upper or lower triangular band matrix A, stored in the
first KD+1 rows of the array. The j-th column of A is stored
in the j-th column of the array AB as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
X       (input/output) REAL array, dimension (N)
On entry, the right hand side b of the triangular system.
On exit, X is overwritten by the solution vector x.
.TP 8
SCALE   (output) REAL
The scaling factor s for the triangular system
A * x = s*b  or  A\(aq* x = s*b.
If SCALE = 0, the matrix A is singular or badly scaled, and
the vector x is an exact or approximate solution to A*x = 0.
.TP 8
CNORM   (input or output) REAL array, dimension (N)

If NORMIN = \(aqY\(aq, CNORM is an input argument and CNORM(j)
contains the norm of the off-diagonal part of the j-th column
of A.  If TRANS = \(aqN\(aq, CNORM(j) must be greater than or equal
to the infinity-norm, and if TRANS = \(aqT\(aq or \(aqC\(aq, CNORM(j)
must be greater than or equal to the 1-norm.

If NORMIN = \(aqN\(aq, CNORM is an output argument and CNORM(j)
returns the 1-norm of the offdiagonal part of the j-th column
of A.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -k, the k-th argument had an illegal value
.SH FURTHER DETAILS
A rough bound on x is computed; if that is less than overflow, STBSV
is called, otherwise, specific code is used which checks for possible
overflow or divide-by-zero at every operation.
.br

A columnwise scheme is used for solving A*x = b.  The basic algorithm
if A is lower triangular is
.br

     x[1:n] := b[1:n]
.br
     for j = 1, ..., n
.br
          x(j) := x(j) / A(j,j)
.br
          x[j+1:n] := x[j+1:n] - x(j) * A[j+1:n,j]
.br
     end
.br

Define bounds on the components of x after j iterations of the loop:
   M(j) = bound on x[1:j]
.br
   G(j) = bound on x[j+1:n]
.br
Initially, let M(0) = 0 and G(0) = max{x(i), i=1,...,n}.
.br

Then for iteration j+1 we have
.br
   M(j+1) <= G(j) / | A(j+1,j+1) |
.br
   G(j+1) <= G(j) + M(j+1) * | A[j+2:n,j+1] |
.br
          <= G(j) ( 1 + CNORM(j+1) / | A(j+1,j+1) | )
.br

where CNORM(j+1) is greater than or equal to the infinity-norm of
column j+1 of A, not counting the diagonal.  Hence
.br

   G(j) <= G(0) product ( 1 + CNORM(i) / | A(i,i) | )
.br
                1<=i<=j
.br
and
.br

   |x(j)| <= ( G(0) / |A(j,j)| ) product ( 1 + CNORM(i) / |A(i,i)| )
                                 1<=i< j
.br

Since |x(j)| <= M(j), we use the Level 2 BLAS routine STBSV if the
reciprocal of the largest M(j), j=1,..,n, is larger than
.br
max(underflow, 1/overflow).
.br

The bound on x(j) is also used to determine when a step in the
columnwise method can be performed without fear of overflow.  If
the computed bound is greater than a large constant, x is scaled to
prevent overflow, but if the bound overflows, x is set to 0, x(j) to
1, and scale to 0, and a non-trivial solution to A*x = 0 is found.

Similarly, a row-wise scheme is used to solve A\(aq*x = b.  The basic
algorithm for A upper triangular is
.br

     for j = 1, ..., n
.br
          x(j) := ( b(j) - A[1:j-1,j]\(aq * x[1:j-1] ) / A(j,j)
     end
.br

We simultaneously compute two bounds
.br
     G(j) = bound on ( b(i) - A[1:i-1,i]\(aq * x[1:i-1] ), 1<=i<=j
     M(j) = bound on x(i), 1<=i<=j
.br

The initial values are G(0) = 0, M(0) = max{b(i), i=1,..,n}, and we
add the constraint G(j) >= G(j-1) and M(j) >= M(j-1) for j >= 1.
Then the bound on x(j) is
.br

     M(j) <= M(j-1) * ( 1 + CNORM(j) ) / | A(j,j) |
.br

          <= M(0) * product ( ( 1 + CNORM(i) ) / |A(i,i)| )
                    1<=i<=j
.br

and we can safely call STBSV if 1/M(n) and 1/G(n) are both greater
than max(underflow, 1/overflow).
.br

