.TH  ZLAQGE 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLAQGE - a general M by N matrix A using the row and column scaling factors in the vectors R and C
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAQGE(
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
DOUBLE
PRECISION AMAX, COLCND, ROWCND
.TP 19
.ti +4
DOUBLE
PRECISION C( * ), R( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * )
.SH PURPOSE
ZLAQGE equilibrates a general M by N matrix A using the row and
column scaling factors in the vectors R and C.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the M by N matrix A.
On exit, the equilibrated matrix.  See EQUED for the form of
the equilibrated matrix.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(M,1).
.TP 8
R       (input) DOUBLE PRECISION array, dimension (M)
The row scale factors for A.
.TP 8
C       (input) DOUBLE PRECISION array, dimension (N)
The column scale factors for A.
.TP 8
ROWCND  (input) DOUBLE PRECISION
Ratio of the smallest R(i) to the largest R(i).
.TP 8
COLCND  (input) DOUBLE PRECISION
Ratio of the smallest C(i) to the largest C(i).
.TP 8
AMAX    (input) DOUBLE PRECISION
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
