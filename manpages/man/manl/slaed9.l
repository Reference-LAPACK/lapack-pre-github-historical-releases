.TH  SLAED9 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLAED9 - the roots of the secular equation, as defined by the values in D, Z, and RHO, between KSTART and KSTOP
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAED9(
K, KSTART, KSTOP, N, D, Q, LDQ, RHO, DLAMDA, W,
S, LDS, INFO )
.TP 19
.ti +4
INTEGER
INFO, K, KSTART, KSTOP, LDQ, LDS, N
.TP 19
.ti +4
REAL
RHO
.TP 19
.ti +4
REAL
D( * ), DLAMDA( * ), Q( LDQ, * ), S( LDS, * ),
W( * )
.SH PURPOSE
SLAED9 finds the roots of the secular equation, as defined by the
values in D, Z, and RHO, between KSTART and KSTOP.  It makes the
appropriate calls to SLAED4 and then stores the new matrix of
eigenvectors for use in calculating the next level of Z vectors.

.SH ARGUMENTS
.TP 8
K       (input) INTEGER
The number of terms in the rational function to be solved by
SLAED4.  K >= 0.
.TP 8
KSTART  (input) INTEGER
KSTOP   (input) INTEGER
The updated eigenvalues Lambda(I), KSTART <= I <= KSTOP
are to be computed.  1 <= KSTART <= KSTOP <= K.
.TP 8
N       (input) INTEGER
The number of rows and columns in the Q matrix.
N >= K (delation may result in N > K).
.TP 8
D       (output) REAL array, dimension (N)
D(I) contains the updated eigenvalues
for KSTART <= I <= KSTOP.
.TP 8
Q       (workspace) REAL array, dimension (LDQ,N)
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  LDQ >= max( 1, N ).
.TP 8
RHO     (input) REAL
The value of the parameter in the rank one update equation.
RHO >= 0 required.
.TP 8
DLAMDA  (input) REAL array, dimension (K)
The first K elements of this array contain the old roots
of the deflated updating problem.  These are the poles
of the secular equation.
.TP 8
W       (input) REAL array, dimension (K)
The first K elements of this array contain the components
of the deflation-adjusted updating vector.
.TP 8
S       (output) REAL array, dimension (LDS, K)
Will contain the eigenvectors of the repaired matrix which
will be stored for subsequent Z vector calculation and
multiplied by the previously accumulated eigenvectors
to update the system.
.TP 8
LDS     (input) INTEGER
The leading dimension of S.  LDS >= max( 1, K ).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = 1, an eigenvalue did not converge
.SH FURTHER DETAILS
Based on contributions by
.br
   Jeff Rutter, Computer Science Division, University of California
   at Berkeley, USA
.br

