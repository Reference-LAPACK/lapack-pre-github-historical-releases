.TH  CGETRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CGETRS - a system of linear equations  A * X = B, A**T * X = B, or A**H * X = B with a general N-by-N matrix A using the LU factorization computed by CGETRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CGETRS(
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
COMPLEX
A( LDA, * ), B( LDB, * )
.SH PURPOSE
CGETRS solves a system of linear equations
   A * X = B,  A**T * X = B,  or  A**H * X = B
with a general N-by-N matrix A using the LU factorization computed
by CGETRF.

.SH ARGUMENTS
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
A       (input) COMPLEX array, dimension (LDA,N)
The factors L and U from the factorization A = P*L*U
as computed by CGETRF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices from CGETRF; for 1<=i<=N, row i of the
matrix was interchanged with row IPIV(i).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
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
