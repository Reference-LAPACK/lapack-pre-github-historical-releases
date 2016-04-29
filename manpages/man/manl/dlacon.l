.TH  DLACON 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLACON - the 1-norm of a square, real matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE DLACON(
N, V, X, ISGN, EST, KASE )
.TP 19
.ti +4
INTEGER
KASE, N
.TP 19
.ti +4
DOUBLE
PRECISION EST
.TP 19
.ti +4
INTEGER
ISGN( * )
.TP 19
.ti +4
DOUBLE
PRECISION V( * ), X( * )
.SH PURPOSE
DLACON estimates the 1-norm of a square, real matrix A.
Reverse communication is used for evaluating matrix-vector products.

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
The order of the matrix.  N >= 1.
.TP 7
V      (workspace) DOUBLE PRECISION array, dimension (N)
On the final return, V = A*W,  where  EST = norm(V)/norm(W)
(W is not returned).
.TP 7
X      (input/output) DOUBLE PRECISION array, dimension (N)
On an intermediate return, X should be overwritten by
A * X,   if KASE=1,
A\(aq * X,  if KASE=2,
and DLACON must be re-called with all the other parameters
unchanged.
.TP 7
ISGN   (workspace) INTEGER array, dimension (N)
.TP 7
EST    (input/output) DOUBLE PRECISION
On entry with KASE = 1 or 2 and JUMP = 3, EST should be
unchanged from the previous call to DLACON.
On exit, EST is an estimate (a lower bound) for norm(A). 
.TP 7
KASE   (input/output) INTEGER
On the initial call to DLACON, KASE should be 0.
On an intermediate return, KASE will be 1 or 2, indicating
whether X should be overwritten by A * X  or A\(aq * X.
On the final return from DLACON, KASE will again be 0.
.SH FURTHER DETAILS
Contributed by Nick Higham, University of Manchester.
.br
Originally named SONEST, dated March 16, 1988.
.br

Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of
a real or complex matrix, with applications to condition estimation",
ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.

