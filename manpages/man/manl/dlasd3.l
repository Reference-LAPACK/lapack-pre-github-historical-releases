.TH  DLASD3 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASD3 - all the square roots of the roots of the secular equation, as defined by the values in D and Z
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASD3(
NL, NR, SQRE, K, D, Q, LDQ, DSIGMA, U, LDU, U2,
LDU2, VT, LDVT, VT2, LDVT2, IDXC, CTOT, Z,
INFO )
.TP 19
.ti +4
INTEGER
INFO, K, LDQ, LDU, LDU2, LDVT, LDVT2, NL, NR,
SQRE
.TP 19
.ti +4
INTEGER
CTOT( * ), IDXC( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), DSIGMA( * ), Q( LDQ, * ), U( LDU, * ),
U2( LDU2, * ), VT( LDVT, * ), VT2( LDVT2, * ),
Z( * )
.SH PURPOSE
DLASD3 finds all the square roots of the roots of the secular
equation, as defined by the values in D and Z.  It makes the
appropriate calls to DLASD4 and then updates the singular
vectors by matrix multiplication.
.br

This code makes very mild assumptions about floating point
arithmetic. It will work on machines with a guard digit in
add/subtract, or on those binary machines without guard digits
which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
.br

DLASD3 is called from DLASD1.
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

The bidiagonal matrix has N = NL + NR + 1 rows and
M = N + SQRE >= N columns.
.TP 7
K      (input) INTEGER
The size of the secular equation, 1 =< K = < N.
.TP 7
D      (output) DOUBLE PRECISION array, dimension(K)
On exit the square roots of the roots of the secular equation,
in ascending order.
.TP 7
Q      (workspace) DOUBLE PRECISION array,
dimension at least (LDQ,K).
.TP 7
LDQ    (input) INTEGER
The leading dimension of the array Q.  LDQ >= K.

DSIGMA (input) DOUBLE PRECISION array, dimension(K)
The first K elements of this array contain the old roots
of the deflated updating problem.  These are the poles
of the secular equation.
.TP 7
U      (output) DOUBLE PRECISION array, dimension (LDU, N)
The last N - K columns of this matrix contain the deflated
left singular vectors.
.TP 7
LDU    (input) INTEGER
The leading dimension of the array U.  LDU >= N.
.TP 7
U2     (input/output) DOUBLE PRECISION array, dimension (LDU2, N)
The first K columns of this matrix contain the non-deflated
left singular vectors for the split problem.
.TP 7
LDU2   (input) INTEGER
The leading dimension of the array U2.  LDU2 >= N.
.TP 7
VT     (output) DOUBLE PRECISION array, dimension (LDVT, M)
The last M - K columns of VT\(aq contain the deflated
right singular vectors.
.TP 7
LDVT   (input) INTEGER
The leading dimension of the array VT.  LDVT >= N.
.TP 7
VT2    (input/output) DOUBLE PRECISION array, dimension (LDVT2, N)
The first K columns of VT2\(aq contain the non-deflated
right singular vectors for the split problem.
.TP 7
LDVT2  (input) INTEGER
The leading dimension of the array VT2.  LDVT2 >= N.
.TP 7
IDXC   (input) INTEGER array, dimension ( N )
The permutation used to arrange the columns of U (and rows of
VT) into three groups:  the first group contains non-zero
entries only at and above (or before) NL +1; the second
contains non-zero entries only at and below (or after) NL+2;
and the third is dense. The first column of U and the row of
VT are treated separately, however.

The rows of the singular vectors found by DLASD4
must be likewise permuted before the matrix multiplies can
take place.
.TP 7
CTOT   (input) INTEGER array, dimension ( 4 )
A count of the total number of the various types of columns
in U (or rows in VT), as described in IDXC. The fourth column
type is any column which has been deflated.
.TP 7
Z      (input) DOUBLE PRECISION array, dimension (K)
The first K elements of this array contain the components
of the deflation-adjusted updating row vector.
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

