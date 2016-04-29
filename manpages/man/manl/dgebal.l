.TH  DGEBAL 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DGEBAL - a general real matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE DGEBAL(
JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
.TP 19
.ti +4
CHARACTER
JOB
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), SCALE( * )
.SH PURPOSE
DGEBAL balances a general real matrix A.  This involves, first,
permuting A by a similarity transformation to isolate eigenvalues
in the first 1 to ILO-1 and last IHI+1 to N elements on the
diagonal; and second, applying a diagonal similarity transformation
to rows and columns ILO to IHI to make the rows and columns as
close in norm as possible.  Both steps are optional.
.br

Balancing may reduce the 1-norm of the matrix, and improve the
accuracy of the computed eigenvalues and/or eigenvectors.

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
Specifies the operations to be performed on A:
.br
= \(aqN\(aq:  none:  simply set ILO = 1, IHI = N, SCALE(I) = 1.0
for i = 1,...,N;
= \(aqP\(aq:  permute only;
.br
= \(aqS\(aq:  scale only;
.br
= \(aqB\(aq:  both permute and scale.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the input matrix A.
On exit,  A is overwritten by the balanced matrix.
If JOB = \(aqN\(aq, A is not referenced.
See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
ILO     (output) INTEGER
IHI     (output) INTEGER
ILO and IHI are set to integers such that on exit
A(i,j) = 0 if i > j and j = 1,...,ILO-1 or I = IHI+1,...,N.
If JOB = \(aqN\(aq or \(aqS\(aq, ILO = 1 and IHI = N.
.TP 8
SCALE   (output) DOUBLE PRECISION array, dimension (N)
Details of the permutations and scaling factors applied to
A.  If P(j) is the index of the row and column interchanged
with row and column j and D(j) is the scaling factor
applied to row and column j, then
SCALE(j) = P(j)    for j = 1,...,ILO-1
= D(j)    for j = ILO,...,IHI
= P(j)    for j = IHI+1,...,N.
The order in which the interchanges are made is N to IHI+1,
then 1 to ILO-1.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
The permutations consist of row and column interchanges which put
the matrix in the form
.br

           ( T1   X   Y  )
.br
   P A P = (  0   B   Z  )
.br
           (  0   0   T2 )
.br

where T1 and T2 are upper triangular matrices whose eigenvalues lie
along the diagonal.  The column indices ILO and IHI mark the starting
and ending columns of the submatrix B. Balancing consists of applying
a diagonal similarity transformation inv(D) * B * D to make the
1-norms of each row of B and its corresponding column nearly equal.
The output matrix is
.br

   ( T1     X*D          Y    )
.br
   (  0  inv(D)*B*D  inv(D)*Z ).
.br
   (  0      0           T2   )
.br

Information about the permutations P and the diagonal matrix D is
returned in the vector SCALE.
.br

This subroutine is based on the EISPACK routine BALANC.
.br

Modified by Tzu-Yi Chen, Computer Science Division, University of
  California at Berkeley, USA
.br

