.TH  SLAG2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAG2 - the eigenvalues of a 2 x 2 generalized eigenvalue problem A - w B, with scaling as necessary to avoid over-/underflow
.SH SYNOPSIS
.TP 18
SUBROUTINE SLAG2(
A, LDA, B, LDB, SAFMIN, SCALE1, SCALE2, WR1,
WR2, WI )
.TP 18
.ti +4
INTEGER
LDA, LDB
.TP 18
.ti +4
REAL
SAFMIN, SCALE1, SCALE2, WI, WR1, WR2
.TP 18
.ti +4
REAL
A( LDA, * ), B( LDB, * )
.SH PURPOSE
SLAG2 computes the eigenvalues of a 2 x 2 generalized eigenvalue
problem  A - w B, with scaling as necessary to avoid over-/underflow.

The scaling factor "s" results in a modified eigenvalue equation

    s A - w B
.br

where  s  is a non-negative scaling factor chosen so that  w,  w B,
and  s A  do not overflow and, if possible, do not underflow, either.

.SH ARGUMENTS
.TP 8
A       (input) REAL array, dimension (LDA, 2)
On entry, the 2 x 2 matrix A.  It is assumed that its 1-norm
is less than 1/SAFMIN.  Entries less than
sqrt(SAFMIN)*norm(A) are subject to being treated as zero.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= 2.
.TP 8
B       (input) REAL array, dimension (LDB, 2)
On entry, the 2 x 2 upper triangular matrix B.  It is
assumed that the one-norm of B is less than 1/SAFMIN.  The
diagonals should be at least sqrt(SAFMIN) times the largest
element of B (in absolute value); if a diagonal is smaller
than that, then  +/- sqrt(SAFMIN) will be used instead of
that diagonal.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= 2.
.TP 8
SAFMIN  (input) REAL
The smallest positive number s.t. 1/SAFMIN does not
overflow.  (This should always be SLAMCH(\(aqS\(aq) -- it is an
argument in order to avoid having to call SLAMCH frequently.)
.TP 8
SCALE1  (output) REAL
A scaling factor used to avoid over-/underflow in the
eigenvalue equation which defines the first eigenvalue.  If
the eigenvalues are complex, then the eigenvalues are
( WR1  +/-  WI i ) / SCALE1  (which may lie outside the
exponent range of the machine), SCALE1=SCALE2, and SCALE1
will always be positive.  If the eigenvalues are real, then
the first (real) eigenvalue is  WR1 / SCALE1 , but this may
overflow or underflow, and in fact, SCALE1 may be zero or
less than the underflow threshhold if the exact eigenvalue
is sufficiently large.
.TP 8
SCALE2  (output) REAL
A scaling factor used to avoid over-/underflow in the
eigenvalue equation which defines the second eigenvalue.  If
the eigenvalues are complex, then SCALE2=SCALE1.  If the
eigenvalues are real, then the second (real) eigenvalue is
WR2 / SCALE2 , but this may overflow or underflow, and in
fact, SCALE2 may be zero or less than the underflow
threshhold if the exact eigenvalue is sufficiently large.
.TP 8
WR1     (output) REAL
If the eigenvalue is real, then WR1 is SCALE1 times the
eigenvalue closest to the (2,2) element of A B**(-1).  If the
eigenvalue is complex, then WR1=WR2 is SCALE1 times the real
part of the eigenvalues.
.TP 8
WR2     (output) REAL
If the eigenvalue is real, then WR2 is SCALE2 times the
other eigenvalue.  If the eigenvalue is complex, then
WR1=WR2 is SCALE1 times the real part of the eigenvalues.
.TP 8
WI      (output) REAL
If the eigenvalue is real, then WI is zero.  If the
eigenvalue is complex, then WI is SCALE1 times the imaginary
part of the eigenvalues.  WI will always be non-negative.
