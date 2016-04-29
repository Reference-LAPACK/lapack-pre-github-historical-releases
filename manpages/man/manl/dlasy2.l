.TH  DLASY2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASY2 - for the N1 by N2 matrix X, 1 <= N1,N2 <= 2, in   op(TL)*X + ISGN*X*op(TR) = SCALE*B,
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASY2(
LTRANL, LTRANR, ISGN, N1, N2, TL, LDTL, TR,
LDTR, B, LDB, SCALE, X, LDX, XNORM, INFO )
.TP 19
.ti +4
LOGICAL
LTRANL, LTRANR
.TP 19
.ti +4
INTEGER
INFO, ISGN, LDB, LDTL, LDTR, LDX, N1, N2
.TP 19
.ti +4
DOUBLE
PRECISION SCALE, XNORM
.TP 19
.ti +4
DOUBLE
PRECISION B( LDB, * ), TL( LDTL, * ), TR( LDTR, * ),
X( LDX, * )
.SH PURPOSE
DLASY2 solves for the N1 by N2 matrix X, 1 <= N1,N2 <= 2, in

where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or
-1.  op(T) = T or T\(aq, where T\(aq denotes the transpose of T.

.SH ARGUMENTS
.TP 8
LTRANL  (input) LOGICAL
On entry, LTRANL specifies the op(TL):
= .FALSE., op(TL) = TL,
= .TRUE., op(TL) = TL\(aq.
.TP 8
LTRANR  (input) LOGICAL
On entry, LTRANR specifies the op(TR):
= .FALSE., op(TR) = TR,
= .TRUE., op(TR) = TR\(aq.
.TP 8
ISGN    (input) INTEGER
On entry, ISGN specifies the sign of the equation
as described before. ISGN may only be 1 or -1.
.TP 8
N1      (input) INTEGER
On entry, N1 specifies the order of matrix TL.
N1 may only be 0, 1 or 2.
.TP 8
N2      (input) INTEGER
On entry, N2 specifies the order of matrix TR.
N2 may only be 0, 1 or 2.
.TP 8
TL      (input) DOUBLE PRECISION array, dimension (LDTL,2)
On entry, TL contains an N1 by N1 matrix.
.TP 8
LDTL    (input) INTEGER
The leading dimension of the matrix TL. LDTL >= max(1,N1).
.TP 8
TR      (input) DOUBLE PRECISION array, dimension (LDTR,2)
On entry, TR contains an N2 by N2 matrix.
.TP 8
LDTR    (input) INTEGER
The leading dimension of the matrix TR. LDTR >= max(1,N2).
.TP 8
B       (input) DOUBLE PRECISION array, dimension (LDB,2)
On entry, the N1 by N2 matrix B contains the right-hand
side of the equation.
.TP 8
LDB     (input) INTEGER
The leading dimension of the matrix B. LDB >= max(1,N1).
.TP 8
SCALE   (output) DOUBLE PRECISION
On exit, SCALE contains the scale factor. SCALE is chosen
less than or equal to 1 to prevent the solution overflowing.
.TP 8
X       (output) DOUBLE PRECISION array, dimension (LDX,2)
On exit, X contains the N1 by N2 solution.
.TP 8
LDX     (input) INTEGER
The leading dimension of the matrix X. LDX >= max(1,N1).
.TP 8
XNORM   (output) DOUBLE PRECISION
On exit, XNORM is the infinity-norm of the solution.
.TP 8
INFO    (output) INTEGER
On exit, INFO is set to
0: successful exit.
.br
1: TL and TR have too close eigenvalues, so TL or
TR is perturbed to get a nonsingular equation.
NOTE: In the interests of speed, this routine does not
check the inputs for errors.
