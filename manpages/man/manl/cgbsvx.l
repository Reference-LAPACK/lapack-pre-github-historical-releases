.TH  CGBSVX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CGBSVX - the LU factorization to compute the solution to a complex system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE CGBSVX(
FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,
LDAFB, IPIV, EQUED, R, C, B, LDB, X, LDX,
RCOND, FERR, BERR, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
EQUED, FACT, TRANS
.TP 19
.ti +4
INTEGER
INFO, KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
.TP 19
.ti +4
REAL
RCOND
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
REAL
BERR( * ), C( * ), FERR( * ), R( * ),
RWORK( * )
.TP 19
.ti +4
COMPLEX
AB( LDAB, * ), AFB( LDAFB, * ), B( LDB, * ),
WORK( * ), X( LDX, * )
.SH PURPOSE
CGBSVX uses the LU factorization to compute the solution to a complex
system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
where A is a band matrix of order N with KL subdiagonals and KU
superdiagonals, and X and B are N-by-NRHS matrices.
.br

Error bounds on the solution and a condition estimate are also
provided.
.br

.SH DESCRIPTION
The following steps are performed by this subroutine:
.br

1. If FACT = \(aqE\(aq, real scaling factors are computed to equilibrate
   the system:
.br
      TRANS = \(aqN\(aq:  diag(R)*A*diag(C)     *inv(diag(C))*X = diag(R)*B
      TRANS = \(aqT\(aq: (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B
      TRANS = \(aqC\(aq: (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B
   Whether or not the system will be equilibrated depends on the
   scaling of the matrix A, but if equilibration is used, A is
   overwritten by diag(R)*A*diag(C) and B by diag(R)*B (if TRANS=\(aqN\(aq)
   or diag(C)*B (if TRANS = \(aqT\(aq or \(aqC\(aq).
.br

2. If FACT = \(aqN\(aq or \(aqE\(aq, the LU decomposition is used to factor the
   matrix A (after equilibration if FACT = \(aqE\(aq) as
.br
      A = L * U,
.br
   where L is a product of permutation and unit lower triangular
   matrices with KL subdiagonals, and U is upper triangular with
   KL+KU superdiagonals.
.br

3. If some U(i,i)=0, so that U is exactly singular, then the routine
   returns with INFO = i. Otherwise, the factored form of A is used
   to estimate the condition number of the matrix A.  If the
   reciprocal of the condition number is less than machine precision,
   INFO = N+1 is returned as a warning, but the routine still goes on
   to solve for X and compute error bounds as described below.

4. The system of equations is solved for X using the factored form
   of A.
.br

5. Iterative refinement is applied to improve the computed solution
   matrix and calculate error bounds and backward error estimates
   for it.
.br

6. If equilibration was used, the matrix X is premultiplied by
   diag(C) (if TRANS = \(aqN\(aq) or diag(R) (if TRANS = \(aqT\(aq or \(aqC\(aq) so
   that it solves the original system before equilibration.

.SH ARGUMENTS
.TP 8
FACT    (input) CHARACTER*1
Specifies whether or not the factored form of the matrix A is
supplied on entry, and if not, whether the matrix A should be
equilibrated before it is factored.
= \(aqF\(aq:  On entry, AFB and IPIV contain the factored form of
A.  If EQUED is not \(aqN\(aq, the matrix A has been
equilibrated with scaling factors given by R and C.
AB, AFB, and IPIV are not modified.
= \(aqN\(aq:  The matrix A will be copied to AFB and factored.
.br
= \(aqE\(aq:  The matrix A will be equilibrated if necessary, then
copied to AFB and factored.
.TP 8
TRANS   (input) CHARACTER*1
Specifies the form of the system of equations.
= \(aqN\(aq:  A * X = B     (No transpose)
.br
= \(aqT\(aq:  A**T * X = B  (Transpose)
.br
= \(aqC\(aq:  A**H * X = B  (Conjugate transpose)
.TP 8
N       (input) INTEGER
The number of linear equations, i.e., the order of the
matrix A.  N >= 0.
.TP 8
KL      (input) INTEGER
The number of subdiagonals within the band of A.  KL >= 0.
.TP 8
KU      (input) INTEGER
The number of superdiagonals within the band of A.  KU >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices B and X.  NRHS >= 0.
.TP 8
AB      (input/output) COMPLEX array, dimension (LDAB,N)
On entry, the matrix A in band storage, in rows 1 to KL+KU+1.
The j-th column of A is stored in the j-th column of the
array AB as follows:
AB(KU+1+i-j,j) = A(i,j) for max(1,j-KU)<=i<=min(N,j+kl)

If FACT = \(aqF\(aq and EQUED is not \(aqN\(aq, then A must have been
equilibrated by the scaling factors in R and/or C.  AB is not
modified if FACT = \(aqF\(aq or \(aqN\(aq, or if FACT = \(aqE\(aq and
EQUED = \(aqN\(aq on exit.

On exit, if EQUED .ne. \(aqN\(aq, A is scaled as follows:
EQUED = \(aqR\(aq:  A := diag(R) * A
.br
EQUED = \(aqC\(aq:  A := A * diag(C)
.br
EQUED = \(aqB\(aq:  A := diag(R) * A * diag(C).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KL+KU+1.
.TP 8
AFB     (input or output) COMPLEX array, dimension (LDAFB,N)
If FACT = \(aqF\(aq, then AFB is an input argument and on entry
contains details of the LU factorization of the band matrix
A, as computed by CGBTRF.  U is stored as an upper triangular
band matrix with KL+KU superdiagonals in rows 1 to KL+KU+1,
and the multipliers used during the factorization are stored
in rows KL+KU+2 to 2*KL+KU+1.  If EQUED .ne. \(aqN\(aq, then AFB is
the factored form of the equilibrated matrix A.

If FACT = \(aqN\(aq, then AFB is an output argument and on exit
returns details of the LU factorization of A.

If FACT = \(aqE\(aq, then AFB is an output argument and on exit
returns details of the LU factorization of the equilibrated
matrix A (see the description of AB for the form of the
equilibrated matrix).
.TP 8
LDAFB   (input) INTEGER
The leading dimension of the array AFB.  LDAFB >= 2*KL+KU+1.
.TP 8
IPIV    (input or output) INTEGER array, dimension (N)
If FACT = \(aqF\(aq, then IPIV is an input argument and on entry
contains the pivot indices from the factorization A = L*U
as computed by CGBTRF; row i of the matrix was interchanged
with row IPIV(i).

If FACT = \(aqN\(aq, then IPIV is an output argument and on exit
contains the pivot indices from the factorization A = L*U
of the original matrix A.

If FACT = \(aqE\(aq, then IPIV is an output argument and on exit
contains the pivot indices from the factorization A = L*U
of the equilibrated matrix A.
.TP 8
EQUED   (input or output) CHARACTER*1
Specifies the form of equilibration that was done.
= \(aqN\(aq:  No equilibration (always true if FACT = \(aqN\(aq).
.br
= \(aqR\(aq:  Row equilibration, i.e., A has been premultiplied by
diag(R).
= \(aqC\(aq:  Column equilibration, i.e., A has been postmultiplied
by diag(C).
= \(aqB\(aq:  Both row and column equilibration, i.e., A has been
replaced by diag(R) * A * diag(C).
EQUED is an input argument if FACT = \(aqF\(aq; otherwise, it is an
output argument.
.TP 8
R       (input or output) REAL array, dimension (N)
The row scale factors for A.  If EQUED = \(aqR\(aq or \(aqB\(aq, A is
multiplied on the left by diag(R); if EQUED = \(aqN\(aq or \(aqC\(aq, R
is not accessed.  R is an input argument if FACT = \(aqF\(aq;
otherwise, R is an output argument.  If FACT = \(aqF\(aq and
EQUED = \(aqR\(aq or \(aqB\(aq, each element of R must be positive.
.TP 8
C       (input or output) REAL array, dimension (N)
The column scale factors for A.  If EQUED = \(aqC\(aq or \(aqB\(aq, A is
multiplied on the right by diag(C); if EQUED = \(aqN\(aq or \(aqR\(aq, C
is not accessed.  C is an input argument if FACT = \(aqF\(aq;
otherwise, C is an output argument.  If FACT = \(aqF\(aq and
EQUED = \(aqC\(aq or \(aqB\(aq, each element of C must be positive.
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit,
if EQUED = \(aqN\(aq, B is not modified;
if TRANS = \(aqN\(aq and EQUED = \(aqR\(aq or \(aqB\(aq, B is overwritten by
diag(R)*B;
if TRANS = \(aqT\(aq or \(aqC\(aq and EQUED = \(aqC\(aq or \(aqB\(aq, B is
overwritten by diag(C)*B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (output) COMPLEX array, dimension (LDX,NRHS)
If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X
to the original system of equations.  Note that A and B are
modified on exit if EQUED .ne. \(aqN\(aq, and the solution to the
equilibrated system is inv(diag(C))*X if TRANS = \(aqN\(aq and
EQUED = \(aqC\(aq or \(aqB\(aq, or inv(diag(R))*X if TRANS = \(aqT\(aq or \(aqC\(aq
and EQUED = \(aqR\(aq or \(aqB\(aq.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
RCOND   (output) REAL
The estimate of the reciprocal condition number of the matrix
A after equilibration (if done).  If RCOND is less than the
machine precision (in particular, if RCOND = 0), the matrix
is singular to working precision.  This condition is
indicated by a return code of INFO > 0.
.TP 8
FERR    (output) REAL array, dimension (NRHS)
The estimated forward error bound for each solution vector
X(j) (the j-th column of the solution matrix X).
If XTRUE is the true solution corresponding to X(j), FERR(j)
is an estimated upper bound for the magnitude of the largest
element in (X(j) - XTRUE) divided by the magnitude of the
largest element in X(j).  The estimate is as reliable as
the estimate for RCOND, and is almost always a slight
overestimate of the true error.
.TP 8
BERR    (output) REAL array, dimension (NRHS)
The componentwise relative backward error of each solution
vector X(j) (i.e., the smallest relative change in
any element of A or B that makes X(j) an exact solution).
.TP 8
WORK    (workspace) COMPLEX array, dimension (2*N)
.TP 8
RWORK   (workspace/output) REAL array, dimension (N)
On exit, RWORK(1) contains the reciprocal pivot growth
factor norm(A)/norm(U). The "max absolute element" norm is
used. If RWORK(1) is much less than 1, then the stability
of the LU factorization of the (equilibrated) matrix A
could be poor. This also means that the solution X, condition
estimator RCOND, and forward error bound FERR could be
unreliable. If factorization fails with 0<INFO<=N, then
RWORK(1) contains the reciprocal pivot growth factor for the
leading INFO columns of A.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, and i is
.br
<= N:  U(i,i) is exactly zero.  The factorization
has been completed, but the factor U is exactly
singular, so the solution and error bounds
could not be computed. RCOND = 0 is returned.
= N+1: U is nonsingular, but RCOND is less than machine
precision, meaning that the matrix is singular
to working precision.  Nevertheless, the
solution and error bounds are computed because
there are a number of situations where the
computed solution can be more accurate than the
value of RCOND would suggest.
