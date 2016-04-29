.TH  SLAEIN 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAEIN - inverse iteration to find a right or left eigenvector corresponding to the eigenvalue (WR,WI) of a real upper Hessenberg matrix H
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAEIN(
RIGHTV, NOINIT, N, H, LDH, WR, WI, VR, VI, B,
LDB, WORK, EPS3, SMLNUM, BIGNUM, INFO )
.TP 19
.ti +4
LOGICAL
NOINIT, RIGHTV
.TP 19
.ti +4
INTEGER
INFO, LDB, LDH, N
.TP 19
.ti +4
REAL
BIGNUM, EPS3, SMLNUM, WI, WR
.TP 19
.ti +4
REAL
B( LDB, * ), H( LDH, * ), VI( * ), VR( * ),
WORK( * )
.SH PURPOSE
SLAEIN uses inverse iteration to find a right or left eigenvector
corresponding to the eigenvalue (WR,WI) of a real upper Hessenberg
matrix H.

.SH ARGUMENTS
.TP 9
RIGHTV   (input) LOGICAL
= .TRUE. : compute right eigenvector;
.br
= .FALSE.: compute left eigenvector.
.TP 9
NOINIT   (input) LOGICAL
.br
= .TRUE. : no initial vector supplied in (VR,VI).
.br
= .FALSE.: initial vector supplied in (VR,VI).
.TP 8
N       (input) INTEGER
The order of the matrix H.  N >= 0.
.TP 8
H       (input) REAL array, dimension (LDH,N)
The upper Hessenberg matrix H.
.TP 8
LDH     (input) INTEGER
The leading dimension of the array H.  LDH >= max(1,N).
.TP 8
WR      (input) REAL
WI      (input) REAL
The real and imaginary parts of the eigenvalue of H whose
corresponding right or left eigenvector is to be computed.
.TP 8
VR      (input/output) REAL array, dimension (N)
VI      (input/output) REAL array, dimension (N)
On entry, if NOINIT = .FALSE. and WI = 0.0, VR must contain
a real starting vector for inverse iteration using the real
eigenvalue WR; if NOINIT = .FALSE. and WI.ne.0.0, VR and VI
must contain the real and imaginary parts of a complex
starting vector for inverse iteration using the complex
eigenvalue (WR,WI); otherwise VR and VI need not be set.
On exit, if WI = 0.0 (real eigenvalue), VR contains the
computed real eigenvector; if WI.ne.0.0 (complex eigenvalue),
VR and VI contain the real and imaginary parts of the
computed complex eigenvector. The eigenvector is normalized
so that the component of largest magnitude has magnitude 1;
here the magnitude of a complex number (x,y) is taken to be
|x| + |y|.
VI is not referenced if WI = 0.0.
.TP 8
B       (workspace) REAL array, dimension (LDB,N)
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= N+1.
.TP 7
WORK   (workspace) REAL array, dimension (N)
.TP 8
EPS3    (input) REAL
A small machine-dependent value which is used to perturb
close eigenvalues, and to replace zero pivots.
.TP 8
SMLNUM  (input) REAL
A machine-dependent value close to the underflow threshold.
.TP 8
BIGNUM  (input) REAL
A machine-dependent value close to the overflow threshold.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
= 1:  inverse iteration did not converge; VR is set to the
last iterate, and so is VI if WI.ne.0.0.
