.TH  ZHEEV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZHEEV - all eigenvalues and, optionally, eigenvectors of a complex Hermitian matrix A
.SH SYNOPSIS
.TP 18
SUBROUTINE ZHEEV(
JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK,
INFO )
.TP 18
.ti +4
CHARACTER
JOBZ, UPLO
.TP 18
.ti +4
INTEGER
INFO, LDA, LWORK, N
.TP 18
.ti +4
DOUBLE
PRECISION RWORK( * ), W( * )
.TP 18
.ti +4
COMPLEX*16
A( LDA, * ), WORK( * )
.SH PURPOSE
ZHEEV computes all eigenvalues and, optionally, eigenvectors of a
complex Hermitian matrix A.

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
= \(aqN\(aq:  Compute eigenvalues only;
.br
= \(aqV\(aq:  Compute eigenvalues and eigenvectors.
.TP 8
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA, N)
On entry, the Hermitian matrix A.  If UPLO = \(aqU\(aq, the
leading N-by-N upper triangular part of A contains the
upper triangular part of the matrix A.  If UPLO = \(aqL\(aq,
the leading N-by-N lower triangular part of A contains
the lower triangular part of the matrix A.
On exit, if JOBZ = \(aqV\(aq, then if INFO = 0, A contains the
orthonormal eigenvectors of the matrix A.
If JOBZ = \(aqN\(aq, then on exit the lower triangle (if UPLO=\(aqL\(aq)
or the upper triangle (if UPLO=\(aqU\(aq) of A, including the
diagonal, is destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of the array WORK.  LWORK >= max(1,2*N-1).
For optimal efficiency, LWORK >= (NB+1)*N,
where NB is the blocksize for ZHETRD returned by ILAENV.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (max(1, 3*N-2))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of an intermediate tridiagonal
form did not converge to zero.
