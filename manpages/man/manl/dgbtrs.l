.TH  DGBTRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DGBTRS - a system of linear equations  A * X = B or A\(aq * X = B with a general band matrix A using the LU factorization computed by DGBTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE DGBTRS(
TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,
INFO )
.TP 19
.ti +4
CHARACTER
TRANS
.TP 19
.ti +4
INTEGER
INFO, KL, KU, LDAB, LDB, N, NRHS
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION AB( LDAB, * ), B( LDB, * )
.SH PURPOSE
DGBTRS solves a system of linear equations
   A * X = B  or  A\(aq * X = B
with a general band matrix A using the LU factorization computed
by DGBTRF.

.SH ARGUMENTS
.TP 8
TRANS   (input) CHARACTER*1
Specifies the form of the system of equations.
= \(aqN\(aq:  A * X = B  (No transpose)
.br
= \(aqT\(aq:  A\(aq* X = B  (Transpose)
.br
= \(aqC\(aq:  A\(aq* X = B  (Conjugate transpose = Transpose)
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KL      (input) INTEGER
The number of subdiagonals within the band of A.  KL >= 0.
.TP 8
KU      (input) INTEGER
The number of superdiagonals within the band of A.  KU >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
Details of the LU factorization of the band matrix A, as
computed by DGBTRF.  U is stored as an upper triangular band
matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
the multipliers used during the factorization are stored in
rows KL+KU+2 to 2*KL+KU+1.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices; for 1 <= i <= N, row i of the matrix was
interchanged with row IPIV(i).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit, the solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
