.TH  SLASD1 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLASD1 - the SVD of an upper bidiagonal N-by-M matrix B,
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASD1(
NL, NR, SQRE, D, ALPHA, BETA, U, LDU, VT, LDVT,
IDXQ, IWORK, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDU, LDVT, NL, NR, SQRE
.TP 19
.ti +4
REAL
ALPHA, BETA
.TP 19
.ti +4
INTEGER
IDXQ( * ), IWORK( * )
.TP 19
.ti +4
REAL
D( * ), U( LDU, * ), VT( LDVT, * ), WORK( * )
.SH PURPOSE
SLASD1 computes the SVD of an upper bidiagonal N-by-M matrix B,
where N = NL + NR + 1 and M = N + SQRE. SLASD1 is called from SLASD0.

A related subroutine SLASD7 handles the case in which the singular
values (and the singular vectors in factored form) are desired.

SLASD1 computes the SVD as follows:
.br

              ( D1(in)  0    0     0 )
.br
  B = U(in) * (   Z1\(aq   a   Z2\(aq    b ) * VT(in)
.br
              (   0     0   D2(in) 0 )
.br

    = U(out) * ( D(out) 0) * VT(out)
.br

where Z\(aq = (Z1\(aq a Z2\(aq b) = u\(aq VT\(aq, and u is a vector of dimension M
with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
elsewhere; and the entry b is empty if SQRE = 0.
.br

The left singular vectors of the original matrix are stored in U, and
the transpose of the right singular vectors are stored in VT, and the
singular values are in D.  The algorithm consists of three stages:

   The first stage consists of deflating the size of the problem
   when there are multiple singular values or when there are zeros in
   the Z vector.  For each such occurence the dimension of the
   secular equation problem is reduced by one.  This stage is
   performed by the routine SLASD2.
.br

   The second stage consists of calculating the updated
.br
   singular values. This is done by finding the square roots of the
   roots of the secular equation via the routine SLASD4 (as called
   by SLASD3). This routine also calculates the singular vectors of
   the current problem.
.br

   The final stage consists of computing the updated singular vectors
   directly using the updated singular values.  The singular vectors
   for the current problem are multiplied with the singular vectors
   from the overall problem.
.br

.SH ARGUMENTS
.TP 7
NL     (input) INTEGER
The row dimension of the upper block.  NL >= 1.
.TP 7
NR     (input) INTEGER
The row dimension of the lower block.  NR >= 1.
.TP 7
SQRE   (input) INTEGER
= 0: the lower block is an NR-by-NR square matrix.
.br
= 1: the lower block is an NR-by-(NR+1) rectangular matrix.

The bidiagonal matrix has row dimension N = NL + NR + 1,
and column dimension M = N + SQRE.
.TP 7
D      (input/output) REAL array, dimension (NL+NR+1).
N = NL+NR+1
On entry D(1:NL,1:NL) contains the singular values of the
.br
upper block; and D(NL+2:N) contains the singular values of
.br
the lower block. On exit D(1:N) contains the singular values
of the modified matrix.
.TP 7
ALPHA  (input/output) REAL
Contains the diagonal element associated with the added row.
.TP 7
BETA   (input/output) REAL
Contains the off-diagonal element associated with the added
row.
.TP 7
U      (input/output) REAL array, dimension (LDU,N)
On entry U(1:NL, 1:NL) contains the left singular vectors of
.br
the upper block; U(NL+2:N, NL+2:N) contains the left singular
vectors of the lower block. On exit U contains the left
singular vectors of the bidiagonal matrix.
.TP 7
LDU    (input) INTEGER
The leading dimension of the array U.  LDU >= max( 1, N ).
.TP 7
VT     (input/output) REAL array, dimension (LDVT,M)
where M = N + SQRE.
On entry VT(1:NL+1, 1:NL+1)\(aq contains the right singular
.br
vectors of the upper block; VT(NL+2:M, NL+2:M)\(aq contains
the right singular vectors of the lower block. On exit
VT\(aq contains the right singular vectors of the
bidiagonal matrix.
.TP 7
LDVT   (input) INTEGER
The leading dimension of the array VT.  LDVT >= max( 1, M ).
.TP 6
IDXQ  (output) INTEGER array, dimension (N)
This contains the permutation which will reintegrate the
subproblem just solved back into sorted order, i.e.
D( IDXQ( I = 1, N ) ) will be in ascending order.
.TP 7
IWORK  (workspace) INTEGER array, dimension (4*N)
.TP 7
WORK   (workspace) REAL array, dimension (3*M**2+2*M)
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = 1, an singular value did not converge
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Huan Ren, Computer Science Division, University of
   California at Berkeley, USA
.br

