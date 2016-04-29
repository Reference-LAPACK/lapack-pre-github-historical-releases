.TH  ZGGHRD 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZGGHRD - a pair of complex matrices (A,B) to generalized upper Hessenberg form using unitary transformations, where A is a general matrix and B is upper triangular
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGGHRD(
COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,
LDQ, Z, LDZ, INFO )
.TP 19
.ti +4
CHARACTER
COMPQ, COMPZ
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
Z( LDZ, * )
.SH PURPOSE
ZGGHRD reduces a pair of complex matrices (A,B) to generalized upper
Hessenberg form using unitary transformations, where A is a
general matrix and B is upper triangular.  The form of the
generalized eigenvalue problem is
.br
   A*x = lambda*B*x,
.br
and B is typically made upper triangular by computing its QR
factorization and moving the unitary matrix Q to the left side
of the equation.
.br

This subroutine simultaneously reduces A to a Hessenberg matrix H:
   Q**H*A*Z = H
.br
and transforms B to another upper triangular matrix T:
.br
   Q**H*B*Z = T
.br
in order to reduce the problem to its standard form
.br
   H*y = lambda*T*y
.br
where y = Z**H*x.
.br

The unitary matrices Q and Z are determined as products of Givens
rotations.  They may either be formed explicitly, or they may be
postmultiplied into input matrices Q1 and Z1, so that
.br
     Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
.br
     Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
.br
If Q1 is the unitary matrix from the QR factorization of B in the
original equation A*x = lambda*B*x, then ZGGHRD reduces the original
problem to generalized Hessenberg form.
.br

.SH ARGUMENTS
.TP 8
COMPQ   (input) CHARACTER*1
= \(aqN\(aq: do not compute Q;
.br
= \(aqI\(aq: Q is initialized to the unit matrix, and the
unitary matrix Q is returned;
= \(aqV\(aq: Q must contain a unitary matrix Q1 on entry,
and the product Q1*Q is returned.
.TP 8
COMPZ   (input) CHARACTER*1
= \(aqN\(aq: do not compute Q;
.br
= \(aqI\(aq: Q is initialized to the unit matrix, and the
unitary matrix Q is returned;
= \(aqV\(aq: Q must contain a unitary matrix Q1 on entry,
and the product Q1*Q is returned.
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
ILO and IHI mark the rows and columns of A which are to be
reduced.  It is assumed that A is already upper triangular
in rows and columns 1:ILO-1 and IHI+1:N.  ILO and IHI are
normally set by a previous call to ZGGBAL; otherwise they
should be set to 1 and N respectively.
1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA, N)
On entry, the N-by-N general matrix to be reduced.
On exit, the upper triangle and the first subdiagonal of A
are overwritten with the upper Hessenberg matrix H, and the
rest is set to zero.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB, N)
On entry, the N-by-N upper triangular matrix B.
On exit, the upper triangular matrix T = Q**H B Z.  The
elements below the diagonal are set to zero.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
Q       (input/output) COMPLEX*16 array, dimension (LDQ, N)
On entry, if COMPQ = \(aqV\(aq, the unitary matrix Q1, typically
from the QR factorization of B.
On exit, if COMPQ=\(aqI\(aq, the unitary matrix Q, and if
COMPQ = \(aqV\(aq, the product Q1*Q.
Not referenced if COMPQ=\(aqN\(aq.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.
LDQ >= N if COMPQ=\(aqV\(aq or \(aqI\(aq; LDQ >= 1 otherwise.
.TP 8
Z       (input/output) COMPLEX*16 array, dimension (LDZ, N)
On entry, if COMPZ = \(aqV\(aq, the unitary matrix Z1.
On exit, if COMPZ=\(aqI\(aq, the unitary matrix Z, and if
COMPZ = \(aqV\(aq, the product Z1*Z.
Not referenced if COMPZ=\(aqN\(aq.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.
LDZ >= N if COMPZ=\(aqV\(aq or \(aqI\(aq; LDZ >= 1 otherwise.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
This routine reduces A to Hessenberg and B to triangular form by
an unblocked reduction, as described in _Matrix_Computations_,
by Golub and van Loan (Johns Hopkins Press).
.br

