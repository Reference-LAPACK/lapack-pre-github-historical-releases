.TH  SLAQGB 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAQGB - a general M by N band matrix A with KL subdiagonals and KU superdiagonals using the row and scaling factors in the vectors R and C
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAQGB(
M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,
AMAX, EQUED )
.TP 19
.ti +4
CHARACTER
EQUED
.TP 19
.ti +4
INTEGER
KL, KU, LDAB, M, N
.TP 19
.ti +4
REAL
AMAX, COLCND, ROWCND
.TP 19
.ti +4
REAL
AB( LDAB, * ), C( * ), R( * )
.SH PURPOSE
SLAQGB equilibrates a general M by N band matrix A with KL
subdiagonals and KU superdiagonals using the row and scaling factors
in the vectors R and C.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
KL      (input) INTEGER
The number of subdiagonals within the band of A.  KL >= 0.
.TP 8
KU      (input) INTEGER
The number of superdiagonals within the band of A.  KU >= 0.
.TP 8
AB      (input/output) REAL array, dimension (LDAB,N)
On entry, the matrix A in band storage, in rows 1 to KL+KU+1.
The j-th column of A is stored in the j-th column of the
array AB as follows:
AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)

On exit, the equilibrated matrix, in the same storage format
as A.  See EQUED for the form of the equilibrated matrix.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDA >= KL+KU+1.
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
