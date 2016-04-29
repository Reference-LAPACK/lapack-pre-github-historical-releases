.TH  SBDSDC 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SBDSDC - the singular value decomposition (SVD) of a real N-by-N (upper or lower) bidiagonal matrix B
.SH SYNOPSIS
.TP 19
SUBROUTINE SBDSDC(
UPLO, COMPQ, N, D, E, U, LDU, VT, LDVT, Q, IQ,
WORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
COMPQ, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDU, LDVT, N
.TP 19
.ti +4
INTEGER
IQ( * ), IWORK( * )
.TP 19
.ti +4
REAL
D( * ), E( * ), Q( * ), U( LDU, * ),
VT( LDVT, * ), WORK( * )
.SH PURPOSE
SBDSDC computes the singular value decomposition (SVD) of a real
N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
using a divide and conquer method, where S is a diagonal matrix
with non-negative diagonal elements (the singular values of B), and
U and VT are orthogonal matrices of left and right singular vectors,
respectively. SBDSDC can be used to compute all singular values,
and optionally, singular vectors or singular vectors in compact form.

This code makes very mild assumptions about floating point
arithmetic. It will work on machines with a guard digit in
add/subtract, or on those binary machines without guard digits
which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.  See SLASD3 for details.

The code currently calls SLASDQ if singular values only are desired.
However, it can be slightly modified to compute singular values
using the divide and conquer method.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  B is upper bidiagonal.
.br
= \(aqL\(aq:  B is lower bidiagonal.
.TP 8
COMPQ   (input) CHARACTER*1
Specifies whether singular vectors are to be computed
as follows:
.br
= \(aqN\(aq:  Compute singular values only;
.br
= \(aqP\(aq:  Compute singular values and compute singular
vectors in compact form;
= \(aqI\(aq:  Compute singular values and singular vectors.
.TP 8
N       (input) INTEGER
The order of the matrix B.  N >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the n diagonal elements of the bidiagonal matrix B.
On exit, if INFO=0, the singular values of B.
.TP 8
E       (input/output) REAL array, dimension (N-1)
On entry, the elements of E contain the offdiagonal
elements of the bidiagonal matrix whose SVD is desired.
On exit, E has been destroyed.
.TP 8
U       (output) REAL array, dimension (LDU,N)
If  COMPQ = \(aqI\(aq, then:
On exit, if INFO = 0, U contains the left singular vectors
of the bidiagonal matrix.
For other values of COMPQ, U is not referenced.
.TP 8
LDU     (input) INTEGER
The leading dimension of the array U.  LDU >= 1.
If singular vectors are desired, then LDU >= max( 1, N ).
.TP 8
VT      (output) REAL array, dimension (LDVT,N)
If  COMPQ = \(aqI\(aq, then:
On exit, if INFO = 0, VT\(aq contains the right singular
vectors of the bidiagonal matrix.
For other values of COMPQ, VT is not referenced.
.TP 8
LDVT    (input) INTEGER
The leading dimension of the array VT.  LDVT >= 1.
If singular vectors are desired, then LDVT >= max( 1, N ).
.TP 8
Q       (output) REAL array, dimension (LDQ)
If  COMPQ = \(aqP\(aq, then:
On exit, if INFO = 0, Q and IQ contain the left
and right singular vectors in a compact form,
requiring O(N log N) space instead of 2*N**2.
In particular, Q contains all the REAL data in
LDQ >= N*(11 + 2*SMLSIZ + 8*INT(LOG_2(N/(SMLSIZ+1))))
words of memory, where SMLSIZ is returned by ILAENV and
is equal to the maximum size of the subproblems at the
bottom of the computation tree (usually about 25).
For other values of COMPQ, Q is not referenced.
.TP 8
IQ      (output) INTEGER array, dimension (LDIQ)
If  COMPQ = \(aqP\(aq, then:
On exit, if INFO = 0, Q and IQ contain the left
and right singular vectors in a compact form,
requiring O(N log N) space instead of 2*N**2.
In particular, IQ contains all INTEGER data in
LDIQ >= N*(3 + 3*INT(LOG_2(N/(SMLSIZ+1))))
words of memory, where SMLSIZ is returned by ILAENV and
is equal to the maximum size of the subproblems at the
bottom of the computation tree (usually about 25).
For other values of COMPQ, IQ is not referenced.
.TP 8
WORK    (workspace) REAL array, dimension (MAX(1,LWORK))
If COMPQ = \(aqN\(aq then LWORK >= (4 * N).
If COMPQ = \(aqP\(aq then LWORK >= (6 * N).
If COMPQ = \(aqI\(aq then LWORK >= (3 * N**2 + 4 * N).
.TP 8
IWORK   (workspace) INTEGER array, dimension (8*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  The algorithm failed to compute an singular value.
The update process of divide and conquer failed.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Huan Ren, Computer Science Division, University of
   California at Berkeley, USA
.br
