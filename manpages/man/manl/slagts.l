.TH  SLAGTS 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAGTS - be used to solve one of the systems of equations   (T - lambda*I)*x = y or (T - lambda*I)\(aq*x = y,
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAGTS(
JOB, N, A, B, C, D, IN, Y, TOL, INFO )
.TP 19
.ti +4
INTEGER
INFO, JOB, N
.TP 19
.ti +4
REAL
TOL
.TP 19
.ti +4
INTEGER
IN( * )
.TP 19
.ti +4
REAL
A( * ), B( * ), C( * ), D( * ), Y( * )
.SH PURPOSE
SLAGTS may be used to solve one of the systems of equations

where T is an n by n tridiagonal matrix, for x, following the
factorization of (T - lambda*I) as
.br

   (T - lambda*I) = P*L*U ,
.br

by routine SLAGTF. The choice of equation to be solved is
controlled by the argument JOB, and in each case there is an option
to perturb zero or very small diagonal elements of U, this option
being intended for use in applications such as inverse iteration.

.SH ARGUMENTS
.TP 8
JOB     (input) INTEGER
Specifies the job to be performed by SLAGTS as follows:
.br
=  1: The equations  (T - lambda*I)x = y  are to be solved,
but diagonal elements of U are not to be perturbed.
= -1: The equations  (T - lambda*I)x = y  are to be solved
and, if overflow would otherwise occur, the diagonal
elements of U are to be perturbed. See argument TOL
below.
=  2: The equations  (T - lambda*I)\(aqx = y  are to be solved,
but diagonal elements of U are not to be perturbed.
= -2: The equations  (T - lambda*I)\(aqx = y  are to be solved
and, if overflow would otherwise occur, the diagonal
elements of U are to be perturbed. See argument TOL
below.
.TP 8
N       (input) INTEGER
The order of the matrix T.
.TP 8
A       (input) REAL array, dimension (N)
On entry, A must contain the diagonal elements of U as
returned from SLAGTF.
.TP 8
B       (input) REAL array, dimension (N-1)
On entry, B must contain the first super-diagonal elements of
U as returned from SLAGTF.
.TP 8
C       (input) REAL array, dimension (N-1)
On entry, C must contain the sub-diagonal elements of L as
returned from SLAGTF.
.TP 8
D       (input) REAL array, dimension (N-2)
On entry, D must contain the second super-diagonal elements
of U as returned from SLAGTF.
.TP 8
IN      (input) INTEGER array, dimension (N)
On entry, IN must contain details of the matrix P as returned
from SLAGTF.
.TP 8
Y       (input/output) REAL array, dimension (N)
On entry, the right hand side vector y.
On exit, Y is overwritten by the solution vector x.
.TP 8
TOL     (input/output) REAL
On entry, with  JOB .lt. 0, TOL should be the minimum
perturbation to be made to very small diagonal elements of U.
TOL should normally be chosen as about eps*norm(U), where eps
is the relative machine precision, but if TOL is supplied as
non-positive, then it is reset to eps*max( abs( u(i,j) ) ).
If  JOB .gt. 0  then TOL is not referenced.

On exit, TOL is changed as described above, only if TOL is
non-positive on entry. Otherwise TOL is unchanged.
.TP 8
INFO    (output) INTEGER
= 0   : successful exit
.br
.lt. 0: if INFO = -i, the i-th argument had an illegal value
.br
.gt. 0: overflow would occur when computing the INFO(th)
element of the solution vector x. This can only occur
when JOB is supplied as positive and either means
that a diagonal element of U is very small, or that
the elements of the right-hand side vector y are very
large.
