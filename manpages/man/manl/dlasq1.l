.TH  DLASQ1 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DLASQ1 - the singular values of a real N-by-N bidiagonal matrix with diagonal D and off-diagonal E
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASQ1(
N, D, E, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), WORK( * )
.SH PURPOSE
DLASQ1 computes the singular values of a real N-by-N bidiagonal
matrix with diagonal D and off-diagonal E. The singular values
are computed to high relative accuracy, in the absence of
denormalization, underflow and overflow. The algorithm was first
presented in
.br

"Accurate singular values and differential qd algorithms" by K. V.
Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
1994,
.br

and the present implementation is described in "An implementation of
the dqds Algorithm (Positive Case)", LAPACK Working Note.

.SH ARGUMENTS
.TP 6
N     (input) INTEGER
The number of rows and columns in the matrix. N >= 0.
.TP 6
D     (input/output) DOUBLE PRECISION array, dimension (N)
On entry, D contains the diagonal elements of the
bidiagonal matrix whose SVD is desired. On normal exit,
D contains the singular values in decreasing order.
.TP 6
E     (input/output) DOUBLE PRECISION array, dimension (N)
On entry, elements E(1:N-1) contain the off-diagonal elements
of the bidiagonal matrix whose SVD is desired.
On exit, E is overwritten.
.TP 6
WORK  (workspace) DOUBLE PRECISION array, dimension (4*N)
.TP 6
INFO  (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: the algorithm failed
= 1, a split was marked by a positive value in E
= 2, current block of Z not diagonalized after 30*N
iterations (in inner while loop)
= 3, termination criterion of outer while loop not met 
(program created more than N unreduced blocks)
