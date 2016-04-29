.TH  DGGBAL 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DGGBAL - a pair of general real matrices (A,B)
.SH SYNOPSIS
.TP 19
SUBROUTINE DGGBAL(
JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,
RSCALE, WORK, INFO )
.TP 19
.ti +4
CHARACTER
JOB
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LDB, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), LSCALE( * ),
RSCALE( * ), WORK( * )
.SH PURPOSE
DGGBAL balances a pair of general real matrices (A,B).  This
involves, first, permuting A and B by similarity transformations to
isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
elements on the diagonal; and second, applying a diagonal similarity
transformation to rows and columns ILO to IHI to make the rows
and columns as close in norm as possible. Both steps are optional.

Balancing may reduce the 1-norm of the matrices, and improve the
accuracy of the computed eigenvalues and/or eigenvectors in the
generalized eigenvalue problem A*x = lambda*B*x.
.br

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
Specifies the operations to be performed on A and B:
.br
= \(aqN\(aq:  none:  simply set ILO = 1, IHI = N, LSCALE(I) = 1.0
and RSCALE(I) = 1.0 for i = 1,...,N.
= \(aqP\(aq:  permute only;
.br
= \(aqS\(aq:  scale only;
.br
= \(aqB\(aq:  both permute and scale.
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the input matrix A.
On exit,  A is overwritten by the balanced matrix.
If JOB = \(aqN\(aq, A is not referenced.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
On entry, the input matrix B.
On exit,  B is overwritten by the balanced matrix.
If JOB = \(aqN\(aq, B is not referenced.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
ILO     (output) INTEGER
IHI     (output) INTEGER
ILO and IHI are set to integers such that on exit
A(i,j) = 0 and B(i,j) = 0 if i > j and
j = 1,...,ILO-1 or i = IHI+1,...,N.
If JOB = \(aqN\(aq or \(aqS\(aq, ILO = 1 and IHI = N.
.TP 8
LSCALE  (output) DOUBLE PRECISION array, dimension (N)
Details of the permutations and scaling factors applied
to the left side of A and B.  If P(j) is the index of the
row interchanged with row j, and D(j)
is the scaling factor applied to row j, then
LSCALE(j) = P(j)    for J = 1,...,ILO-1
= D(j)    for J = ILO,...,IHI
= P(j)    for J = IHI+1,...,N.
The order in which the interchanges are made is N to IHI+1,
then 1 to ILO-1.
.TP 8
RSCALE  (output) DOUBLE PRECISION array, dimension (N)
Details of the permutations and scaling factors applied
to the right side of A and B.  If P(j) is the index of the
column interchanged with column j, and D(j)
is the scaling factor applied to column j, then
LSCALE(j) = P(j)    for J = 1,...,ILO-1
= D(j)    for J = ILO,...,IHI
= P(j)    for J = IHI+1,...,N.
The order in which the interchanges are made is N to IHI+1,
then 1 to ILO-1.
.TP 8
WORK    (workspace) REAL array, dimension (lwork)
lwork must be at least max(1,6*N) when JOB = \(aqS\(aq or \(aqB\(aq, and
at least 1 when JOB = \(aqN\(aq or \(aqP\(aq.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
See R.C. WARD, Balancing the generalized eigenvalue problem,
               SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.

