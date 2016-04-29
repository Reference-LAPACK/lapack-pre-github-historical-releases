.TH  CGEBAK 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CGEBAK - the right or left eigenvectors of a complex general matrix by backward transformation on the computed eigenvectors of the balanced matrix output by CGEBAL
.SH SYNOPSIS
.TP 19
SUBROUTINE CGEBAK(
JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,
INFO )
.TP 19
.ti +4
CHARACTER
JOB, SIDE
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDV, M, N
.TP 19
.ti +4
REAL
SCALE( * )
.TP 19
.ti +4
COMPLEX
V( LDV, * )
.SH PURPOSE
CGEBAK forms the right or left eigenvectors of a complex general
matrix by backward transformation on the computed eigenvectors of the
balanced matrix output by CGEBAL.

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
Specifies the type of backward transformation required:
= \(aqN\(aq, do nothing, return immediately;
= \(aqP\(aq, do backward transformation for permutation only;
= \(aqS\(aq, do backward transformation for scaling only;
= \(aqB\(aq, do backward transformations for both permutation and
scaling.
JOB must be the same as the argument JOB supplied to CGEBAL.
.TP 8
SIDE    (input) CHARACTER*1
= \(aqR\(aq:  V contains right eigenvectors;
.br
= \(aqL\(aq:  V contains left eigenvectors.
.TP 8
N       (input) INTEGER
The number of rows of the matrix V.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
The integers ILO and IHI determined by CGEBAL.
1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
.TP 8
SCALE   (input) REAL array, dimension (N)
Details of the permutation and scaling factors, as returned
by CGEBAL.
.TP 8
M       (input) INTEGER
The number of columns of the matrix V.  M >= 0.
.TP 8
V       (input/output) COMPLEX array, dimension (LDV,M)
On entry, the matrix of right or left eigenvectors to be
transformed, as returned by CHSEIN or CTREVC.
On exit, V is overwritten by the transformed eigenvectors.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V. LDV >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
