.TH  ZHPSVX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZHPSVX - the diagonal pivoting factorization A = U*D*U**H or A = L*D*L**H to compute the solution to a complex system of linear equations A * X = B, where A is an N-by-N Hermitian matrix stored in packed format and X and B are N-by-NRHS matrices
.SH SYNOPSIS
.TP 19
SUBROUTINE ZHPSVX(
FACT, UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X,
LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
FACT, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDB, LDX, N, NRHS
.TP 19
.ti +4
DOUBLE
PRECISION RCOND
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION BERR( * ), FERR( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX*16
AFP( * ), AP( * ), B( LDB, * ), WORK( * ),
X( LDX, * )
.SH PURPOSE
ZHPSVX uses the diagonal pivoting factorization A = U*D*U**H or
A = L*D*L**H to compute the solution to a complex system of linear
equations A * X = B, where A is an N-by-N Hermitian matrix stored
in packed format and X and B are N-by-NRHS matrices.

Error bounds on the solution and a condition estimate are also
provided.
.br

.SH DESCRIPTION
The following steps are performed:
.br

1. If FACT = \(aqN\(aq, the diagonal pivoting method is used to factor A as
      A = U * D * U**H,  if UPLO = \(aqU\(aq, or
.br
      A = L * D * L**H,  if UPLO = \(aqL\(aq,
.br
   where U (or L) is a product of permutation and unit upper (lower)
   triangular matrices and D is Hermitian and block diagonal with
   1-by-1 and 2-by-2 diagonal blocks.
.br

2. If some D(i,i)=0, so that D is exactly singular, then the routine
   returns with INFO = i. Otherwise, the factored form of A is used
   to estimate the condition number of the matrix A.  If the
   reciprocal of the condition number is less than machine precision,
   INFO = N+1 is returned as a warning, but the routine still goes on
   to solve for X and compute error bounds as described below.

3. The system of equations is solved for X using the factored form
   of A.
.br

4. Iterative refinement is applied to improve the computed solution
   matrix and calculate error bounds and backward error estimates
   for it.
.br

.SH ARGUMENTS
.TP 8
FACT    (input) CHARACTER*1
Specifies whether or not the factored form of A has been
supplied on entry.
= \(aqF\(aq:  On entry, AFP and IPIV contain the factored form of
A.  AFP and IPIV will not be modified.
= \(aqN\(aq:  The matrix A will be copied to AFP and factored.
.TP 8
UPLO    (input) CHARACTER*1
.br
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
of the matrices B and X.  NRHS >= 0.
.TP 8
AP      (input) COMPLEX*16 array, dimension (N*(N+1)/2)
The upper or lower triangle of the Hermitian matrix A, packed
columnwise in a linear array.  The j-th column of A is stored
in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
See below for further details.
.TP 8
AFP     (input or output) COMPLEX*16 array, dimension (N*(N+1)/2)
If FACT = \(aqF\(aq, then AFP is an input argument and on entry
contains the block diagonal matrix D and the multipliers used
to obtain the factor U or L from the factorization
A = U*D*U**H or A = L*D*L**H as computed by ZHPTRF, stored as
a packed triangular matrix in the same storage format as A.

If FACT = \(aqN\(aq, then AFP is an output argument and on exit
contains the block diagonal matrix D and the multipliers used
to obtain the factor U or L from the factorization
A = U*D*U**H or A = L*D*L**H as computed by ZHPTRF, stored as
a packed triangular matrix in the same storage format as A.
.TP 8
IPIV    (input or output) INTEGER array, dimension (N)
If FACT = \(aqF\(aq, then IPIV is an input argument and on entry
contains details of the interchanges and the block structure
of D, as determined by ZHPTRF.
If IPIV(k) > 0, then rows and columns k and IPIV(k) were
interchanged and D(k,k) is a 1-by-1 diagonal block.
If UPLO = \(aqU\(aq and IPIV(k) = IPIV(k-1) < 0, then rows and
columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
is a 2-by-2 diagonal block.  If UPLO = \(aqL\(aq and IPIV(k) =
IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were
interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.

If FACT = \(aqN\(aq, then IPIV is an output argument and on exit
contains details of the interchanges and the block structure
of D, as determined by ZHPTRF.
.TP 8
B       (input) COMPLEX*16 array, dimension (LDB,NRHS)
The N-by-NRHS right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (output) COMPLEX*16 array, dimension (LDX,NRHS)
If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
RCOND   (output) DOUBLE PRECISION
The estimate of the reciprocal condition number of the matrix
A.  If RCOND is less than the machine precision (in
particular, if RCOND = 0), the matrix is singular to working
precision.  This condition is indicated by a return code of
INFO > 0.
.TP 8
FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
The estimated forward error bound for each solution vector
X(j) (the j-th column of the solution matrix X).
If XTRUE is the true solution corresponding to X(j), FERR(j)
is an estimated upper bound for the magnitude of the largest
element in (X(j) - XTRUE) divided by the magnitude of the
largest element in X(j).  The estimate is as reliable as
the estimate for RCOND, and is almost always a slight
overestimate of the true error.
.TP 8
BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
The componentwise relative backward error of each solution
vector X(j) (i.e., the smallest relative change in
any element of A or B that makes X(j) an exact solution).
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (2*N)
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, and i is
.br
<= N:  D(i,i) is exactly zero.  The factorization
has been completed but the factor D is exactly
singular, so the solution and error bounds could
not be computed. RCOND = 0 is returned.
= N+1: D is nonsingular, but RCOND is less than machine
precision, meaning that the matrix is singular
to working precision.  Nevertheless, the
solution and error bounds are computed because
there are a number of situations where the
computed solution can be more accurate than the
value of RCOND would suggest.
.SH FURTHER DETAILS
The packed storage scheme is illustrated by the following example
when N = 4, UPLO = \(aqU\(aq:
.br

Two-dimensional storage of the Hermitian matrix A:
.br

   a11 a12 a13 a14
.br
       a22 a23 a24
.br
           a33 a34     (aij = conjg(aji))
.br
               a44
.br

Packed storage of the upper triangle of A:
.br

AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]

