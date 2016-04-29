.TH  CSPTRS 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CSPTRS - a system of linear equations A*X = B with a complex symmetric matrix A stored in packed format using the factorization A = U*D*U**T or A = L*D*L**T computed by CSPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CSPTRS(
UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
COMPLEX
AP( * ), B( LDB, * )
.SH PURPOSE
CSPTRS solves a system of linear equations A*X = B with a complex
symmetric matrix A stored in packed format using the factorization
A = U*D*U**T or A = L*D*L**T computed by CSPTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the details of the factorization are stored
as an upper or lower triangular matrix.
= \(aqU\(aq:  Upper triangular, form is A = U*D*U**T;
.br
= \(aqL\(aq:  Lower triangular, form is A = L*D*L**T.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
AP      (input) COMPLEX array, dimension (N*(N+1)/2)
The block diagonal matrix D and the multipliers used to
obtain the factor U or L as computed by CSPTRF, stored as a
packed triangular matrix.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D
as determined by CSPTRF.
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
< 0: if INFO = -i, the i-th argument had an illegal value
