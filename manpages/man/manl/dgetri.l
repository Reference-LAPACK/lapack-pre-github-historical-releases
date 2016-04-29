.TH  DGETRI 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DGETRI - the inverse of a matrix using the LU factorization computed by DGETRF
.SH SYNOPSIS
.TP 19
SUBROUTINE DGETRI(
N, A, LDA, IPIV, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, N
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), WORK( * )
.SH PURPOSE
DGETRI computes the inverse of a matrix using the LU factorization
computed by DGETRF.

This method inverts U and then computes inv(A) by solving the system
inv(A)*L = inv(U) for inv(A).
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the factors L and U from the factorization
A = P*L*U as computed by DGETRF.
On exit, if INFO = 0, the inverse of the original matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices from DGETRF; for 1<=i<=N, row i of the
matrix was interchanged with row IPIV(i).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO=0, then WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,N).
For optimal performance LWORK >= N*NB, where NB is
the optimal blocksize returned by ILAENV.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, U(i,i) is exactly zero; the matrix is
singular and its inverse could not be computed.
