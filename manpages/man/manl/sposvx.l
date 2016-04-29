.TH  SPOSVX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
SPOSVX - the Cholesky factorization A = U**T*U or A = L*L**T to compute the solution to a real system of linear equations  A * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE SPOSVX(
FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,
S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,
IWORK, INFO )
.TP 19
.ti +4
CHARACTER
EQUED, FACT, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LDAF, LDB, LDX, N, NRHS
.TP 19
.ti +4
REAL
RCOND
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
BERR( * ), FERR( * ), S( * ), WORK( * ),
X( LDX, * )
.SH PURPOSE
SPOSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
compute the solution to a real system of linear equations
   A * X = B,
where A is an N-by-N symmetric positive definite matrix and X and B
are N-by-NRHS matrices.
.br

Error bounds on the solution and a condition estimate are also
provided.
.br

.SH DESCRIPTION
The following steps are performed:
.br

1. If FACT = \(aqE\(aq, real scaling factors are computed to equilibrate
   the system:
.br
      diag(S) * A * diag(S) * inv(diag(S)) * X = diag(S) * B
   Whether or not the system will be equilibrated depends on the
   scaling of the matrix A, but if equilibration is used, A is
   overwritten by diag(S)*A*diag(S) and B by diag(S)*B.
.br

2. If FACT = \(aqN\(aq or \(aqE\(aq, the Cholesky decomposition is used to
   factor the matrix A (after equilibration if FACT = \(aqE\(aq) as
      A = U**T* U,  if UPLO = \(aqU\(aq, or
.br
      A = L * L**T,  if UPLO = \(aqL\(aq,
.br
   where U is an upper triangular matrix and L is a lower triangular
   matrix.
.br

3. If the leading i-by-i principal minor is not positive definite,
   then the routine returns with INFO = i. Otherwise, the factored
   form of A is used to estimate the condition number of the matrix
   A.  If the reciprocal of the condition number is less than machine
   precision, INFO = N+1 is returned as a warning, but the routine
   still goes on to solve for X and compute error bounds as
   described below.
.br

4. The system of equations is solved for X using the factored form
   of A.
.br

5. Iterative refinement is applied to improve the computed solution
   matrix and calculate error bounds and backward error estimates
   for it.
.br

6. If equilibration was used, the matrix X is premultiplied by
   diag(S) so that it solves the original system before
.br
   equilibration.
.br

.SH ARGUMENTS
.TP 8
FACT    (input) CHARACTER*1
Specifies whether or not the factored form of the matrix A is
supplied on entry, and if not, whether the matrix A should be
equilibrated before it is factored.
= \(aqF\(aq:  On entry, AF contains the factored form of A.
If EQUED = \(aqY\(aq, the matrix A has been equilibrated
with scaling factors given by S.  A and AF will not
be modified.
= \(aqN\(aq:  The matrix A will be copied to AF and factored.
.br
= \(aqE\(aq:  The matrix A will be equilibrated if necessary, then
copied to AF and factored.
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
of the matrices B and X.  NRHS >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the symmetric matrix A, except if FACT = \(aqF\(aq and
EQUED = \(aqY\(aq, then A must contain the equilibrated matrix
diag(S)*A*diag(S).  If UPLO = \(aqU\(aq, the leading
N-by-N upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading N-by-N lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.  A is not modified if
FACT = \(aqF\(aq or \(aqN\(aq, or if FACT = \(aqE\(aq and EQUED = \(aqN\(aq on exit.

On exit, if FACT = \(aqE\(aq and EQUED = \(aqY\(aq, A is overwritten by
diag(S)*A*diag(S).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
AF      (input or output) REAL array, dimension (LDAF,N)
If FACT = \(aqF\(aq, then AF is an input argument and on entry
contains the triangular factor U or L from the Cholesky
factorization A = U**T*U or A = L*L**T, in the same storage
format as A.  If EQUED .ne. \(aqN\(aq, then AF is the factored form
of the equilibrated matrix diag(S)*A*diag(S).

If FACT = \(aqN\(aq, then AF is an output argument and on exit
returns the triangular factor U or L from the Cholesky
factorization A = U**T*U or A = L*L**T of the original
matrix A.

If FACT = \(aqE\(aq, then AF is an output argument and on exit
returns the triangular factor U or L from the Cholesky
factorization A = U**T*U or A = L*L**T of the equilibrated
matrix A (see the description of A for the form of the
equilibrated matrix).
.TP 8
LDAF    (input) INTEGER
The leading dimension of the array AF.  LDAF >= max(1,N).
.TP 8
EQUED   (input or output) CHARACTER*1
Specifies the form of equilibration that was done.
= \(aqN\(aq:  No equilibration (always true if FACT = \(aqN\(aq).
.br
= \(aqY\(aq:  Equilibration was done, i.e., A has been replaced by
diag(S) * A * diag(S).
EQUED is an input argument if FACT = \(aqF\(aq; otherwise, it is an
output argument.
.TP 8
S       (input or output) REAL array, dimension (N)
The scale factors for A; not accessed if EQUED = \(aqN\(aq.  S is
an input argument if FACT = \(aqF\(aq; otherwise, S is an output
argument.  If FACT = \(aqF\(aq and EQUED = \(aqY\(aq, each element of S
must be positive.
.TP 8
B       (input/output) REAL array, dimension (LDB,NRHS)
On entry, the N-by-NRHS right hand side matrix B.
On exit, if EQUED = \(aqN\(aq, B is not modified; if EQUED = \(aqY\(aq,
B is overwritten by diag(S) * B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (output) REAL array, dimension (LDX,NRHS)
If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X to
the original system of equations.  Note that if EQUED = \(aqY\(aq,
A and B are modified on exit, and the solution to the
equilibrated system is inv(diag(S))*X.
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
WORK    (workspace) REAL array, dimension (3*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = i, and i is
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
