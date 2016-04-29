.TH  CSYSV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
CSYSV - the solution to a complex system of linear equations  A * X = B,
.SH SYNOPSIS
.TP 18
SUBROUTINE CSYSV(
UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
LWORK, INFO )
.TP 18
.ti +4
CHARACTER
UPLO
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, N, NRHS
.TP 18
.ti +4
INTEGER
IPIV( * )
.TP 18
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), WORK( * )
.SH PURPOSE
CSYSV computes the solution to a complex system of linear equations
   A * X = B,
where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
matrices.
.br

The diagonal pivoting method is used to factor A as
.br
   A = U * D * U**T,  if UPLO = \(aqU\(aq, or
.br
   A = L * D * L**T,  if UPLO = \(aqL\(aq,
.br
where U (or L) is a product of permutation and unit upper (lower)
triangular matrices, and D is symmetric and block diagonal with 
1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
used to solve the system of equations A * X = B.
.br

.SH ARGUMENTS
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
of the matrix B.  NRHS >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the leading
N-by-N upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading N-by-N lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.

On exit, if INFO = 0, the block diagonal matrix D and the
multipliers used to obtain the factor U or L from the
factorization A = U*D*U**T or A = L*D*L**T as computed by
CSYTRF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (output) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D, as
determined by CSYTRF.  If IPIV(k) > 0, then rows and columns
k and IPIV(k) were interchanged, and D(k,k) is a 1-by-1
diagonal block.  If UPLO = \(aqU\(aq and IPIV(k) = IPIV(k-1) < 0,
then rows and columns k-1 and -IPIV(k) were interchanged and
D(k-1:k,k-1:k) is a 2-by-2 diagonal block.  If UPLO = \(aqL\(aq and
IPIV(k) = IPIV(k+1) < 0, then rows and columns k+1 and
-IPIV(k) were interchanged and D(k:k+1,k:k+1) is a 2-by-2
diagonal block.
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
On entry, the N-by-NRHS right hand side matrix B.
On exit, if INFO = 0, the N-by-NRHS solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of WORK.  LWORK >= 1, and for best performance
LWORK >= max(1,N*NB), where NB is the optimal blocksize for
CSYTRF.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = i, D(i,i) is exactly zero.  The factorization
has been completed, but the block diagonal matrix D is
exactly singular, so the solution could not be computed.
