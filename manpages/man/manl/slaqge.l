.TH  SLAQGE 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAQGE - a general M by N matrix A using the row and column scaling factors in the vectors R and C
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAQGE(
M, N, A, LDA, R, C, ROWCND, COLCND, AMAX,
EQUED )
.TP 19
.ti +4
CHARACTER
EQUED
.TP 19
.ti +4
INTEGER
LDA, M, N
.TP 19
.ti +4
REAL
AMAX, COLCND, ROWCND
.TP 19
.ti +4
REAL
A( LDA, * ), C( * ), R( * )
.SH PURPOSE
SLAQGE equilibrates a general M by N matrix A using the row and
column scaling factors in the vectors R and C.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the M by N matrix A.
On exit, the equilibrated matrix.  See EQUED for the form of
the equilibrated matrix.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(M,1).
.TP 8
R       (input) REAL array, dimension (M)
The row scale factors for A.
.TP 8
C       (input) REAL array, dimension (N)
The column scale factors for A.
.TP 8
ROWCND  (input) REAL
Ratio of the smallest R(i) to the largest R(i).
.TP 8
COLCND  (input) REAL
Ratio of the smallest C(i) to the largest C(i).
.TP 8
AMAX    (input) REAL
Absolute value of largest matrix entry.
.TP 8
EQUED   (output) CHARACTER*1
Specifies the form of equilibration that was done.
= \(aqN\(aq:  No equilibration
.br
= \(aqR\(aq:  Row equilibration, i.e., A has been premultiplied by
diag(R).
= \(aqC\(aq:  Column equilibration, i.e., A has been postmultiplied
by diag(C).
= \(aqB\(aq:  Both row and column equilibration, i.e., A has been
replaced by diag(R) * A * diag(C).
.SH PARAMETERS

THRESH is a threshold value used to decide if row or column scaling
should be done based on the ratio of the row or column scaling
factors.  If ROWCND < THRESH, row scaling is done, and if
COLCND < THRESH, column scaling is done.

LARGE and SMALL are threshold values used to decide if row scaling
should be done based on the absolute size of the largest matrix
element.  If AMAX > LARGE or AMAX < SMALL, row scaling is done.
