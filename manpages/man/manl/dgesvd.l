.TH  DGESVD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DGESVD - the singular value decomposition (SVD) of a real M-by-N matrix A, optionally computing the left and/or right singular vectors
.SH SYNOPSIS
.TP 19
SUBROUTINE DGESVD(
JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBU, JOBVT
.TP 19
.ti +4
INTEGER
INFO, LDA, LDU, LDVT, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), S( * ), U( LDU, * ),
VT( LDVT, * ), WORK( * )
.SH PURPOSE
DGESVD computes the singular value decomposition (SVD) of a real
M-by-N matrix A, optionally computing the left and/or right singular
vectors. The SVD is written

     A = U * SIGMA * transpose(V)
.br

where SIGMA is an M-by-N matrix which is zero except for its
min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
are the singular values of A; they are real and non-negative, and
are returned in descending order.  The first min(m,n) columns of
U and V are the left and right singular vectors of A.
.br

Note that the routine returns V**T, not V.
.br

.SH ARGUMENTS
.TP 8
JOBU    (input) CHARACTER*1
Specifies options for computing all or part of the matrix U:
.br
= \(aqA\(aq:  all M columns of U are returned in array U:
.br
= \(aqS\(aq:  the first min(m,n) columns of U (the left singular
vectors) are returned in the array U;
= \(aqO\(aq:  the first min(m,n) columns of U (the left singular
vectors) are overwritten on the array A;
= \(aqN\(aq:  no columns of U (no left singular vectors) are
computed.
.TP 8
JOBVT   (input) CHARACTER*1
Specifies options for computing all or part of the matrix
V**T:
.br
= \(aqA\(aq:  all N rows of V**T are returned in the array VT;
.br
= \(aqS\(aq:  the first min(m,n) rows of V**T (the right singular
vectors) are returned in the array VT;
= \(aqO\(aq:  the first min(m,n) rows of V**T (the right singular
vectors) are overwritten on the array A;
= \(aqN\(aq:  no rows of V**T (no right singular vectors) are
computed.

JOBVT and JOBU cannot both be \(aqO\(aq.
.TP 8
M       (input) INTEGER
The number of rows of the input matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the input matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit,
if JOBU = \(aqO\(aq,  A is overwritten with the first min(m,n)
columns of U (the left singular vectors,
stored columnwise);
if JOBVT = \(aqO\(aq, A is overwritten with the first min(m,n)
rows of V**T (the right singular vectors,
stored rowwise);
if JOBU .ne. \(aqO\(aq and JOBVT .ne. \(aqO\(aq, the contents of A
are destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
S       (output) DOUBLE PRECISION array, dimension (min(M,N))
The singular values of A, sorted so that S(i) >= S(i+1).
.TP 8
U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
(LDU,M) if JOBU = \(aqA\(aq or (LDU,min(M,N)) if JOBU = \(aqS\(aq.
If JOBU = \(aqA\(aq, U contains the M-by-M orthogonal matrix U;
if JOBU = \(aqS\(aq, U contains the first min(m,n) columns of U
(the left singular vectors, stored columnwise);
if JOBU = \(aqN\(aq or \(aqO\(aq, U is not referenced.
.TP 8
LDU     (input) INTEGER
The leading dimension of the array U.  LDU >= 1; if
JOBU = \(aqS\(aq or \(aqA\(aq, LDU >= M.
.TP 8
VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
If JOBVT = \(aqA\(aq, VT contains the N-by-N orthogonal matrix
V**T;
if JOBVT = \(aqS\(aq, VT contains the first min(m,n) rows of
V**T (the right singular vectors, stored rowwise);
if JOBVT = \(aqN\(aq or \(aqO\(aq, VT is not referenced.
.TP 8
LDVT    (input) INTEGER
The leading dimension of the array VT.  LDVT >= 1; if
JOBVT = \(aqA\(aq, LDVT >= N; if JOBVT = \(aqS\(aq, LDVT >= min(M,N).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
superdiagonal elements of an upper bidiagonal matrix B
whose diagonal is in S (not necessarily sorted). B
satisfies A = U * B * VT, so it has the same singular values
as A, and singular vectors related by U and VT.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
LWORK >= MAX(1,3*MIN(M,N)+MAX(M,N),5*MIN(M,N)).
For good performance, LWORK should generally be larger.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if DBDSQR did not converge, INFO specifies how many
superdiagonals of an intermediate bidiagonal form B
did not converge to zero. See the description of WORK
above for details.
