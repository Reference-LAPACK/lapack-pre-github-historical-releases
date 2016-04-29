.TH  SGETRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SGETRS - a system of linear equations  A * X = B or A\(aq * X = B with a general N-by-N matrix A using the LU factorization computed by SGETRF
.SH SYNOPSIS
.TP 19
SUBROUTINE SGETRS(
TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
.TP 19
.ti +4
CHARACTER
TRANS
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, N, NRHS
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
REAL
A( LDA, * ), B( LDB, * )
.SH PURPOSE
SGETRS solves a system of linear equations
   A * X = B  or  A\(aq * X = B
with a general N-by-N matrix A using the LU factorization computed
by SGETRF.

.SH ARGUMENTS
.TP 8
TRANS   (input) CHARACTER*1
Specifies the form of the system of equations:
.br
= \(aqN\(aq:  A * X = B  (No transpose)
.br
= \(aqT\(aq:  A\(aq* X = B  (Transpose)
.br
= \(aqC\(aq:  A\(aq* X = B  (Conjugate transpose = Transpose)
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
A       (input) REAL array, dimension (LDA,N)
The factors L and U from the factorization A = P*L*U
as computed by SGETRF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices from SGETRF; for 1<=i<=N, row i of the
matrix was interchanged with row IPIV(i).
.TP 8
B       (input/output) REAL array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit, the solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
