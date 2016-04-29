.TH  CGTSVX 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CGTSVX - the LU factorization to compute the solution to a complex system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE CGTSVX(
FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,
DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,
WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
FACT, TRANS
.TP 19
.ti +4
INTEGER
INFO, LDB, LDX, N, NRHS
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
BERR( * ), FERR( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX
B( LDB, * ), D( * ), DF( * ), DL( * ),
DLF( * ), DU( * ), DU2( * ), DUF( * ),
WORK( * ), X( LDX, * )
.SH PURPOSE
CGTSVX uses the LU factorization to compute the solution to a complex
system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
matrices.
.br

Error bounds on the solution and a condition estimate are also
provided.
.br

.SH DESCRIPTION
The following steps are performed:
.br

1. If FACT = \(aqN\(aq, the LU decomposition is used to factor the matrix A
   as A = L * U, where L is a product of permutation and unit lower
   bidiagonal matrices and U is upper triangular with nonzeros in
   only the main diagonal and first two superdiagonals.
.br

2. If some U(i,i)=0, so that U is exactly singular, then the routine
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
= \(aqF\(aq:  DLF, DF, DUF, DU2, and IPIV contain the factored form
of A; DL, D, DU, DLF, DF, DUF, DU2 and IPIV will not
be modified.
= \(aqN\(aq:  The matrix will be copied to DLF, DF, and DUF
and factored.
.TP 8
TRANS   (input) CHARACTER*1
Specifies the form of the system of equations:
.br
= \(aqN\(aq:  A * X = B     (No transpose)
.br
= \(aqT\(aq:  A**T * X = B  (Transpose)
.br
= \(aqC\(aq:  A**H * X = B  (Conjugate transpose)
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
DL      (input) COMPLEX array, dimension (N-1)
The (n-1) subdiagonal elements of A.
.TP 8
D       (input) COMPLEX array, dimension (N)
The n diagonal elements of A.
.TP 8
DU      (input) COMPLEX array, dimension (N-1)
The (n-1) superdiagonal elements of A.
.TP 8
DLF     (input or output) COMPLEX array, dimension (N-1)
If FACT = \(aqF\(aq, then DLF is an input argument and on entry
contains the (n-1) multipliers that define the matrix L from
the LU factorization of A as computed by CGTTRF.

If FACT = \(aqN\(aq, then DLF is an output argument and on exit
contains the (n-1) multipliers that define the matrix L from
the LU factorization of A.
.TP 8
DF      (input or output) COMPLEX array, dimension (N)
If FACT = \(aqF\(aq, then DF is an input argument and on entry
contains the n diagonal elements of the upper triangular
matrix U from the LU factorization of A.

If FACT = \(aqN\(aq, then DF is an output argument and on exit
contains the n diagonal elements of the upper triangular
matrix U from the LU factorization of A.
.TP 8
DUF     (input or output) COMPLEX array, dimension (N-1)
If FACT = \(aqF\(aq, then DUF is an input argument and on entry
contains the (n-1) elements of the first superdiagonal of U.

If FACT = \(aqN\(aq, then DUF is an output argument and on exit
contains the (n-1) elements of the first superdiagonal of U.
.TP 8
DU2     (input or output) COMPLEX array, dimension (N-2)
If FACT = \(aqF\(aq, then DU2 is an input argument and on entry
contains the (n-2) elements of the second superdiagonal of
U.

If FACT = \(aqN\(aq, then DU2 is an output argument and on exit
contains the (n-2) elements of the second superdiagonal of
U.
.TP 8
IPIV    (input or output) INTEGER array, dimension (N)
If FACT = \(aqF\(aq, then IPIV is an input argument and on entry
contains the pivot indices from the LU factorization of A as
computed by CGTTRF.

If FACT = \(aqN\(aq, then IPIV is an output argument and on exit
contains the pivot indices from the LU factorization of A;
row i of the matrix was interchanged with row IPIV(i).
IPIV(i) will always be either i or i+1; IPIV(i) = i indicates
a row interchange was not required.
.TP 8
B       (input) COMPLEX array, dimension (LDB,NRHS)
The N-by-NRHS right hand side matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (output) COMPLEX array, dimension (LDX,NRHS)
If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
RCOND   (output) REAL
The estimate of the reciprocal condition number of the matrix
A.  If RCOND is less than the machine precision (in
particular, if RCOND = 0), the matrix is singular to working
precision.  This condition is indicated by a return code of
INFO > 0.
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
RWORK   (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, and i is
.br
<= N:  U(i,i) is exactly zero.  The factorization
has not been completed unless i = N, but the
factor U is exactly singular, so the solution
and error bounds could not be computed.
RCOND = 0 is returned.
= N+1: U is nonsingular, but RCOND is less than machine
precision, meaning that the matrix is singular
to working precision.  Nevertheless, the
solution and error bounds are computed because
there are a number of situations where the
computed solution can be more accurate than the
value of RCOND would suggest.
