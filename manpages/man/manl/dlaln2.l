.TH  DLALN2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLALN2 - a system of the form (ca A - w D ) X = s B or (ca A\(aq - w D) X = s B with possible scaling ("s") and perturbation of A
.SH SYNOPSIS
.TP 19
SUBROUTINE DLALN2(
LTRANS, NA, NW, SMIN, CA, A, LDA, D1, D2, B,
LDB, WR, WI, X, LDX, SCALE, XNORM, INFO )
.TP 19
.ti +4
LOGICAL
LTRANS
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LDX, NA, NW
.TP 19
.ti +4
DOUBLE
PRECISION CA, D1, D2, SCALE, SMIN, WI, WR, XNORM
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), X( LDX, * )
.SH PURPOSE
DLALN2 solves a system of the form  (ca A - w D ) X = s B
or (ca A\(aq - w D) X = s B   with possible scaling ("s") and
perturbation of A.  (A\(aq means A-transpose.)

A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
real diagonal matrix, w is a real or complex value, and X and B are
NA x 1 matrices -- real if w is real, complex if w is complex.  NA
may be 1 or 2.
.br

If w is complex, X and B are represented as NA x 2 matrices,
the first column of each being the real part and the second
being the imaginary part.
.br

"s" is a scaling factor (.LE. 1), computed by DLALN2, which is
so chosen that X can be computed without overflow.  X is further
scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
than overflow.
.br

If both singular values of (ca A - w D) are less than SMIN,
SMIN*identity will be used instead of (ca A - w D).  If only one
singular value is less than SMIN, one element of (ca A - w D) will be
perturbed enough to make the smallest singular value roughly SMIN.
If both singular values are at least SMIN, (ca A - w D) will not be
perturbed.  In any case, the perturbation will be at most some small
multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
are computed by infinity-norm approximations, and thus will only be
correct to a factor of 2 or so.
.br

Note: all input quantities are assumed to be smaller than overflow
by a reasonable factor.  (See BIGNUM.)
.br

.SH ARGUMENTS
.TP 8
LTRANS  (input) LOGICAL
=.TRUE.:  A-transpose will be used.
.br
=.FALSE.: A will be used (not transposed.)
.TP 8
NA      (input) INTEGER
The size of the matrix A.  It may (only) be 1 or 2.
.TP 8
NW      (input) INTEGER
1 if "w" is real, 2 if "w" is complex.  It may only be 1
or 2.
.TP 8
SMIN    (input) DOUBLE PRECISION
The desired lower bound on the singular values of A.  This
should be a safe distance away from underflow or overflow,
say, between (underflow/machine precision) and  (machine
precision * overflow ).  (See BIGNUM and ULP.)
.TP 8
CA      (input) DOUBLE PRECISION
The coefficient c, which A is multiplied by.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,NA)
The NA x NA matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of A.  It must be at least NA.
.TP 8
D1      (input) DOUBLE PRECISION
The 1,1 element in the diagonal matrix D.
.TP 8
D2      (input) DOUBLE PRECISION
The 2,2 element in the diagonal matrix D.  Not used if NW=1.
.TP 8
B       (input) DOUBLE PRECISION array, dimension (LDB,NW)
The NA x NW matrix B (right-hand side).  If NW=2 ("w" is
complex), column 1 contains the real part of B and column 2
contains the imaginary part.
.TP 8
LDB     (input) INTEGER
The leading dimension of B.  It must be at least NA.
.TP 8
WR      (input) DOUBLE PRECISION
The real part of the scalar "w".
.TP 8
WI      (input) DOUBLE PRECISION
The imaginary part of the scalar "w".  Not used if NW=1.
.TP 8
X       (output) DOUBLE PRECISION array, dimension (LDX,NW)
The NA x NW matrix X (unknowns), as computed by DLALN2.
If NW=2 ("w" is complex), on exit, column 1 will contain
the real part of X and column 2 will contain the imaginary
part.
.TP 8
LDX     (input) INTEGER
The leading dimension of X.  It must be at least NA.
.TP 8
SCALE   (output) DOUBLE PRECISION
The scale factor that B must be multiplied by to insure
that overflow does not occur when computing X.  Thus,
(ca A - w D) X  will be SCALE*B, not B (ignoring
perturbations of A.)  It will be at most 1.
.TP 8
XNORM   (output) DOUBLE PRECISION
The infinity-norm of X, when X is regarded as an NA x NW
real matrix.
.TP 8
INFO    (output) INTEGER
An error flag.  It will be set to zero if no error occurs,
a negative number if an argument is in error, or a positive
number if  ca A - w D  had to be perturbed.
The possible values are:
.br
= 0: No error occurred, and (ca A - w D) did not have to be
perturbed.
= 1: (ca A - w D) had to be perturbed to make its smallest
(or only) singular value greater than SMIN.
NOTE: In the interests of speed, this routine does not
check the inputs for errors.
