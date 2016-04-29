.SH NAME
ZCGESV - the solution to a real system of linear equations  A * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZCGESV(
N, NRHS, A, LDA, IPIV, B, LDB, X, LDX, WORK,
.TP 19
.ti +4
+
SWORK, ITER, INFO)
.TP 19
.ti +4
INTEGER
INFO,ITER,LDA,LDB,LDX,N,NRHS
.TP 19
.ti +4
INTEGER
IPIV(*)
.TP 19
.ti +4
COMPLEX
SWORK(*)
.TP 19
.ti +4
COMPLEX*16
A(LDA,*),B(LDB,*),WORK(N,*),X(LDX,*)
.SH PURPOSE
ZCGESV computes the solution to a real system of linear equations
   A * X = B,
where A is an N-by-N matrix and X and B are N-by-NRHS matrices.

ZCGESV first attempts to factorize the matrix in SINGLE COMPLEX PRECISION
and use this factorization within an iterative refinement procedure to
produce a solution with DOUBLE COMPLEX PRECISION normwise backward error
quality (see below). If the approach fails the method switches to a
DOUBLE COMPLEX PRECISION factorization and solve.
.br

The iterative refinement is not going to be a winning strategy if
the ratio SINGLE PRECISION performance over DOUBLE PRECISION performance
is too small. A reasonable strategy should take the number of right-hand
sides and the size of the matrix into account. This might be done with a 
call to ILAENV in the future. Up to now, we always try iterative refinement.

The iterative refinement process is stopped if
.br
    ITER > ITERMAX
.br
or for all the RHS we have:
.br
    RNRM < SQRT(N)*XNRM*ANRM*EPS*BWDMAX 
.br
where
.br
    o ITER is the number of the current iteration in the iterative
      refinement process
.br
    o RNRM is the infinity-norm of the residual
.br
    o XNRM is the infinity-norm of the solution
.br
    o ANRM is the infinity-operator-norm of the matrix A
.br
    o EPS is the machine epsilon returned by DLAMCH(\(aqEpsilon\(aq)
The value ITERMAX and BWDMAX are fixed to 30 and 1.0D+00 respectively.

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of linear equations, i.e., the order of the
matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
A       (input or input/ouptut) COMPLEX*16 array,
dimension (LDA,N)
On entry, the N-by-N coefficient matrix A.
On exit, if iterative refinement has been successfully used
(INFO.EQ.0 and ITER.GE.0, see description below), then A is
unchanged, if double precision factorization has been used
(INFO.EQ.0 and ITER.LT.0, see description below), then the
array A contains the factors L and U from the factorization
A = P*L*U; the unit diagonal elements of L are not stored.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (output) INTEGER array, dimension (N)
The pivot indices that define the permutation matrix P;
row i of the matrix was interchanged with row IPIV(i).
Corresponds either to the single precision factorization 
(if INFO.EQ.0 and ITER.GE.0) or the double precision 
factorization (if INFO.EQ.0 and ITER.LT.0).
.TP 8
B       (input) COMPLEX*16 array, dimension (LDB,NRHS)
The N-by-NRHS matrix of right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (output) COMPLEX*16 array, dimension (LDX,NRHS)
If INFO = 0, the N-by-NRHS solution matrix X.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (N*NRHS)
This array is used to hold the residual vectors.
.TP 8
SWORK   (workspace) COMPLEX array, dimension (N*(N+NRHS))
This array is used to use the single precision matrix and the 
right-hand sides or solutions in single precision.
.TP 8
ITER    (output) INTEGER
< 0: iterative refinement has failed, double precision
factorization has been performed
-1 : taking into account machine parameters, N, NRHS, it
is a priori not worth working in SINGLE PRECISION
-2 : overflow of an entry when moving from double to
SINGLE PRECISION
-3 : failure of SGETRF
.br
-31: stop the iterative refinement after the 30th
iterations
> 0: iterative refinement has been sucessfully used.
Returns the number of iterations
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, U(i,i) computed in DOUBLE PRECISION is
exactly zero.  The factorization has been completed,
but the factor U is exactly singular, so the solution
could not be computed.

=========
