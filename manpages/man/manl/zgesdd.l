.TH  ZGESDD 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
ZGESDD - the singular value decomposition (SVD) of a complex M-by-N matrix A, optionally computing the left and/or right singular vectors, by using divide-and-conquer method
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGESDD(
JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK,
LWORK, RWORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ
.TP 19
.ti +4
INTEGER
INFO, LDA, LDU, LDVT, LWORK, M, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * ), S( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), U( LDU, * ), VT( LDVT, * ),
WORK( * )
.SH PURPOSE
ZGESDD computes the singular value decomposition (SVD) of a complex
M-by-N matrix A, optionally computing the left and/or right singular
vectors, by using divide-and-conquer method. The SVD is written

     A = U * SIGMA * conjugate-transpose(V)
.br

where SIGMA is an M-by-N matrix which is zero except for its
min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
are the singular values of A; they are real and non-negative, and
are returned in descending order.  The first min(m,n) columns of
U and V are the left and right singular vectors of A.
.br

Note that the routine returns VT = V**H, not V.
.br

The divide and conquer algorithm makes very mild assumptions about
floating point arithmetic. It will work on machines with a guard
digit in add/subtract, or on those binary machines without guard
digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
Cray-2. It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
.br

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
Specifies options for computing all or part of the matrix U:
.br
= \(aqA\(aq:  all M columns of U and all N rows of V**H are
returned in the arrays U and VT;
= \(aqS\(aq:  the first min(M,N) columns of U and the first
min(M,N) rows of V**H are returned in the arrays U
and VT;
= \(aqO\(aq:  If M >= N, the first N columns of U are overwritten
in the array A and all rows of V**H are returned in
the array VT;
otherwise, all columns of U are returned in the
array U and the first M rows of V**H are overwritten
in the array A;
= \(aqN\(aq:  no columns of U or rows of V**H are computed.
.TP 8
M       (input) INTEGER
The number of rows of the input matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the input matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit,
if JOBZ = \(aqO\(aq,  A is overwritten with the first N columns
of U (the left singular vectors, stored
columnwise) if M >= N;
A is overwritten with the first M rows
of V**H (the right singular vectors, stored
rowwise) otherwise.
if JOBZ .ne. \(aqO\(aq, the contents of A are destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
S       (output) DOUBLE PRECISION array, dimension (min(M,N))
The singular values of A, sorted so that S(i) >= S(i+1).
.TP 8
U       (output) COMPLEX*16 array, dimension (LDU,UCOL)
UCOL = M if JOBZ = \(aqA\(aq or JOBZ = \(aqO\(aq and M < N;
UCOL = min(M,N) if JOBZ = \(aqS\(aq.
If JOBZ = \(aqA\(aq or JOBZ = \(aqO\(aq and M < N, U contains the M-by-M
unitary matrix U;
if JOBZ = \(aqS\(aq, U contains the first min(M,N) columns of U
(the left singular vectors, stored columnwise);
if JOBZ = \(aqO\(aq and M >= N, or JOBZ = \(aqN\(aq, U is not referenced.
.TP 8
LDU     (input) INTEGER
The leading dimension of the array U.  LDU >= 1; if
JOBZ = \(aqS\(aq or \(aqA\(aq or JOBZ = \(aqO\(aq and M < N, LDU >= M.
.TP 8
VT      (output) COMPLEX*16 array, dimension (LDVT,N)
If JOBZ = \(aqA\(aq or JOBZ = \(aqO\(aq and M >= N, VT contains the
N-by-N unitary matrix V**H;
if JOBZ = \(aqS\(aq, VT contains the first min(M,N) rows of
V**H (the right singular vectors, stored rowwise);
if JOBZ = \(aqO\(aq and M < N, or JOBZ = \(aqN\(aq, VT is not referenced.
.TP 8
LDVT    (input) INTEGER
The leading dimension of the array VT.  LDVT >= 1; if
JOBZ = \(aqA\(aq or JOBZ = \(aqO\(aq and M >= N, LDVT >= N;
if JOBZ = \(aqS\(aq, LDVT >= min(M,N).
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= 1.
if JOBZ = \(aqN\(aq, LWORK >= 2*min(M,N)+max(M,N).
if JOBZ = \(aqO\(aq,
LWORK >= 2*min(M,N)*min(M,N)+2*min(M,N)+max(M,N).
if JOBZ = \(aqS\(aq or \(aqA\(aq,
LWORK >= min(M,N)*min(M,N)+2*min(M,N)+max(M,N).
For good performance, LWORK should generally be larger.

If LWORK = -1, a workspace query is assumed.  The optimal
size for the WORK array is calculated and stored in WORK(1),
and no other work except argument checking is performed.
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (MAX(1,LRWORK))
If JOBZ = \(aqN\(aq, LRWORK >= 5*min(M,N).
Otherwise, LRWORK >= 5*min(M,N)*min(M,N) + 7*min(M,N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (8*min(M,N))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  The updating process of DBDSDC did not converge.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Huan Ren, Computer Science Division, University of
   California at Berkeley, USA
.br

