.TH  ZLASCL 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLASCL - the M by N complex matrix A by the real scalar CTO/CFROM
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLASCL(
TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
.TP 19
.ti +4
CHARACTER
TYPE
.TP 19
.ti +4
INTEGER
INFO, KL, KU, LDA, M, N
.TP 19
.ti +4
DOUBLE
PRECISION CFROM, CTO
.TP 19
.ti +4
COMPLEX*16
A( LDA, * )
.SH PURPOSE
ZLASCL multiplies the M by N complex matrix A by the real scalar
CTO/CFROM.  This is done without over/underflow as long as the final
result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
A may be full, upper triangular, lower triangular, upper Hessenberg,
or banded.
.br

.SH ARGUMENTS
.TP 8
TYPE    (input) CHARACTER*1
TYPE indices the storage type of the input matrix.
= \(aqG\(aq:  A is a full matrix.
.br
= \(aqL\(aq:  A is a lower triangular matrix.
.br
= \(aqU\(aq:  A is an upper triangular matrix.
.br
= \(aqH\(aq:  A is an upper Hessenberg matrix.
.br
= \(aqB\(aq:  A is a symmetric band matrix with lower bandwidth KL
and upper bandwidth KU and with the only the lower
half stored.
= \(aqQ\(aq:  A is a symmetric band matrix with lower bandwidth KL
and upper bandwidth KU and with the only the upper
half stored.
= \(aqZ\(aq:  A is a band matrix with lower bandwidth KL and upper
bandwidth KU.
.TP 8
KL      (input) INTEGER
The lower bandwidth of A.  Referenced only if TYPE = \(aqB\(aq,
\(aqQ\(aq or \(aqZ\(aq.
.TP 8
KU      (input) INTEGER
The upper bandwidth of A.  Referenced only if TYPE = \(aqB\(aq,
\(aqQ\(aq or \(aqZ\(aq.
.TP 8
CFROM   (input) DOUBLE PRECISION
CTO     (input) DOUBLE PRECISION
The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
without over/underflow if the final result CTO*A(I,J)/CFROM
can be represented without over/underflow.  CFROM must be
nonzero.
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
The matrix to be multiplied by CTO/CFROM.  See TYPE for the
storage type.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
INFO    (output) INTEGER
0  - successful exit
<0 - if INFO = -i, the i-th argument had an illegal value.
