.TH  SLACN2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLACN2 - the 1-norm of a square, real matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE SLACN2(
N, V, X, ISGN, EST, KASE, ISAVE )
.TP 19
.ti +4
INTEGER
KASE, N
.TP 19
.ti +4
REAL
EST
.TP 19
.ti +4
INTEGER
ISGN( * ), ISAVE( 3 )
.TP 19
.ti +4
REAL
V( * ), X( * )
.SH PURPOSE
SLACN2 estimates the 1-norm of a square, real matrix A.
Reverse communication is used for evaluating matrix-vector products.

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
The order of the matrix.  N >= 1.
.TP 7
V      (workspace) REAL array, dimension (N)
On the final return, V = A*W,  where  EST = norm(V)/norm(W)
(W is not returned).
.TP 7
X      (input/output) REAL array, dimension (N)
On an intermediate return, X should be overwritten by
A * X,   if KASE=1,
A\(aq * X,  if KASE=2,
and SLACN2 must be re-called with all the other parameters
unchanged.
.TP 7
ISGN   (workspace) INTEGER array, dimension (N)
.TP 7
EST    (input/output) REAL
On entry with KASE = 1 or 2 and ISAVE(1) = 3, EST should be
unchanged from the previous call to SLACN2.
On exit, EST is an estimate (a lower bound) for norm(A). 
.TP 7
KASE   (input/output) INTEGER
On the initial call to SLACN2, KASE should be 0.
On an intermediate return, KASE will be 1 or 2, indicating
whether X should be overwritten by A * X  or A\(aq * X.
On the final return from SLACN2, KASE will again be 0.
.TP 7
ISAVE  (input/output) INTEGER array, dimension (3)
ISAVE is used to save variables between calls to SLACN2
.SH FURTHER DETAILS
Contributed by Nick Higham, University of Manchester.
.br
Originally named SONEST, dated March 16, 1988.
.br

Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of
a real or complex matrix, with applications to condition estimation",
ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.

This is a thread safe version of SLACON, which uses the array ISAVE
in place of a SAVE statement, as follows:
.br

   SLACON     SLACN2
.br
    JUMP     ISAVE(1)
.br
    J        ISAVE(2)
.br
    ITER     ISAVE(3)
.br

