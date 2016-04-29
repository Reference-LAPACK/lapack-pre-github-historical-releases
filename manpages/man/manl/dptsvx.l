.TH  DPTSVX 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DPTSVX - the factorization A = L*D*L**T to compute the solution to a real system of linear equations A*X = B, where A is an N-by-N symmetric positive definite tridiagonal matrix and X and B are N-by-NRHS matrices
.SH SYNOPSIS
.TP 19
SUBROUTINE DPTSVX(
FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,
RCOND, FERR, BERR, WORK, INFO )
.TP 19
.ti +4
CHARACTER
FACT
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
DOUBLE
PRECISION B( LDB, * ), BERR( * ), D( * ), DF( * ),
E( * ), EF( * ), FERR( * ), WORK( * ),
X( LDX, * )
.SH PURPOSE
DPTSVX uses the factorization A = L*D*L**T to compute the solution
to a real system of linear equations A*X = B, where A is an N-by-N
symmetric positive definite tridiagonal matrix and X and B are
N-by-NRHS matrices.

Error bounds on the solution and a condition estimate are also
provided.
.br

.SH DESCRIPTION
The following steps are performed:
.br

1. If FACT = \(aqN\(aq, the matrix A is factored as A = L*D*L**T, where L
   is a unit lower bidiagonal matrix and D is diagonal.  The
   factorization can also be regarded as having the form
.br
   A = U**T*D*U.
.br

2. If the leading i-by-i principal minor is not positive definite,
   then the routine returns with INFO = i. Otherwise, the factored
   form of A is used to estimate the condition number of the matrix
   A.  If the reciprocal of the condition number is less than machine
   precision, INFO = N+1 is returned as a warning, but the routine
   still goes on to solve for X and compute error bounds as
   described below.
.br

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
= \(aqF\(aq:  On entry, DF and EF contain the factored form of A.
D, E, DF, and EF will not be modified.
= \(aqN\(aq:  The matrix A will be copied to DF and EF and
factored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices B and X.  NRHS >= 0.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The n diagonal elements of the tridiagonal matrix A.
.TP 8
E       (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) subdiagonal elements of the tridiagonal matrix A.
.TP 8
DF      (input or output) DOUBLE PRECISION array, dimension (N)
If FACT = \(aqF\(aq, then DF is an input argument and on entry
contains the n diagonal elements of the diagonal matrix D
from the L*D*L**T factorization of A.
If FACT = \(aqN\(aq, then DF is an output argument and on exit
contains the n diagonal elements of the diagonal matrix D
from the L*D*L**T factorization of A.
.TP 8
EF      (input or output) DOUBLE PRECISION array, dimension (N-1)
If FACT = \(aqF\(aq, then EF is an input argument and on entry
contains the (n-1) subdiagonal elements of the unit
bidiagonal factor L from the L*D*L**T factorization of A.
If FACT = \(aqN\(aq, then EF is an output argument and on exit
contains the (n-1) subdiagonal elements of the unit
bidiagonal factor L from the L*D*L**T factorization of A.
.TP 8
B       (input) DOUBLE PRECISION array, dimension (LDB,NRHS)
The N-by-NRHS right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
If INFO = 0 of INFO = N+1, the N-by-NRHS solution matrix X.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
RCOND   (output) DOUBLE PRECISION
The reciprocal condition number of the matrix A.  If RCOND
is less than the machine precision (in particular, if
RCOND = 0), the matrix is singular to working precision.
This condition is indicated by a return code of INFO > 0.
.TP 8
FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
The forward error bound for each solution vector
X(j) (the j-th column of the solution matrix X).
If XTRUE is the true solution corresponding to X(j), FERR(j)
is an estimated upper bound for the magnitude of the largest
element in (X(j) - XTRUE) divided by the magnitude of the
largest element in X(j).
.TP 8
BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
The componentwise relative backward error of each solution
vector X(j) (i.e., the smallest relative change in any
element of A or B that makes X(j) an exact solution).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, and i is
.br
<= N:  the leading minor of order i of A is
not positive definite, so the factorization
could not be completed, and the solution has not
been computed. RCOND = 0 is returned.
= N+1: U is nonsingular, but RCOND is less than machine
precision, meaning that the matrix is singular
to working precision.  Nevertheless, the
solution and error bounds are computed because
there are a number of situations where the
computed solution can be more accurate than the
value of RCOND would suggest.
