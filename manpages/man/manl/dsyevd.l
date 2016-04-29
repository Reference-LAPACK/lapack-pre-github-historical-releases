.TH  DSYEVD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DSYEVD - all eigenvalues and, optionally, eigenvectors of a real symmetric matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE DSYEVD(
JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK,
LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LIWORK, LWORK, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), W( * ), WORK( * )
.SH PURPOSE
DSYEVD computes all eigenvalues and, optionally, eigenvectors of a
real symmetric matrix A. If eigenvectors are desired, it uses a
divide and conquer algorithm.
.br

The divide and conquer algorithm makes very mild assumptions about
floating point arithmetic. It will work on machines with a guard
digit in add/subtract, or on those binary machines without guard
digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
Cray-2. It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
.br

Because of large use of BLAS of level 3, DSYEVD needs N**2 more
workspace than DSYEVX.
.br

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
A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the
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
WORK    (workspace/output) DOUBLE PRECISION array,
dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If N <= 1,               LWORK must be at least 1.
If JOBZ = \(aqN\(aq and N > 1, LWORK must be at least 2*N+1.
If JOBZ = \(aqV\(aq and N > 1, LWORK must be at least
1 + 6*N + 2*N**2.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal sizes of the WORK and IWORK
arrays, returns these values as the first entries of the WORK
and IWORK arrays, and no error message related to LWORK or
LIWORK is issued by XERBLA.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK.
If N <= 1,                LIWORK must be at least 1.
If JOBZ  = \(aqN\(aq and N > 1, LIWORK must be at least 1.
If JOBZ  = \(aqV\(aq and N > 1, LIWORK must be at least 3 + 5*N.

If LIWORK = -1, then a workspace query is assumed; the
routine only calculates the optimal sizes of the WORK and
IWORK arrays, returns these values as the first entries of
the WORK and IWORK arrays, and no error message related to
LWORK or LIWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i and JOBZ = \(aqN\(aq, then the algorithm failed
to converge; i off-diagonal elements of an intermediate
tridiagonal form did not converge to zero;
if INFO = i and JOBZ = \(aqV\(aq, then the algorithm failed
to compute an eigenvalue while working on the submatrix
lying in rows and columns INFO/(N+1) through
mod(INFO,N+1).
.SH FURTHER DETAILS
Based on contributions by
.br
   Jeff Rutter, Computer Science Division, University of California
   at Berkeley, USA
.br
Modified by Francoise Tisseur, University of Tennessee.
.br

Modified description of INFO. Sven, 16 Feb 05.
.br
