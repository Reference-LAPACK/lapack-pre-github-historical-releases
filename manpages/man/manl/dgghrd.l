.TH  DGGHRD 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DGGHRD - a pair of real matrices (A,B) to generalized upper Hessenberg form using orthogonal transformations, where A is a general matrix and B is upper triangular
.SH SYNOPSIS
.TP 19
SUBROUTINE DGGHRD(
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
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
Z( LDZ, * )
.SH PURPOSE
DGGHRD reduces a pair of real matrices (A,B) to generalized upper
Hessenberg form using orthogonal transformations, where A is a
general matrix and B is upper triangular.  The form of the
generalized eigenvalue problem is
.br
   A*x = lambda*B*x,
.br
and B is typically made upper triangular by computing its QR
factorization and moving the orthogonal matrix Q to the left side
of the equation.
.br

This subroutine simultaneously reduces A to a Hessenberg matrix H:
   Q**T*A*Z = H
.br
and transforms B to another upper triangular matrix T:
.br
   Q**T*B*Z = T
.br
in order to reduce the problem to its standard form
.br
   H*y = lambda*T*y
.br
where y = Z**T*x.
.br

The orthogonal matrices Q and Z are determined as products of Givens
rotations.  They may either be formed explicitly, or they may be
postmultiplied into input matrices Q1 and Z1, so that
.br

     Q1 * A * Z1**T = (Q1*Q) * H * (Z1*Z)**T
.br

     Q1 * B * Z1**T = (Q1*Q) * T * (Z1*Z)**T
.br

If Q1 is the orthogonal matrix from the QR factorization of B in the
original equation A*x = lambda*B*x, then DGGHRD reduces the original
problem to generalized Hessenberg form.
.br

.SH ARGUMENTS
.TP 8
COMPQ   (input) CHARACTER*1
= \(aqN\(aq: do not compute Q;
.br
= \(aqI\(aq: Q is initialized to the unit matrix, and the
orthogonal matrix Q is returned;
= \(aqV\(aq: Q must contain an orthogonal matrix Q1 on entry,
and the product Q1*Q is returned.
.TP 8
COMPZ   (input) CHARACTER*1
= \(aqN\(aq: do not compute Z;
.br
= \(aqI\(aq: Z is initialized to the unit matrix, and the
orthogonal matrix Z is returned;
= \(aqV\(aq: Z must contain an orthogonal matrix Z1 on entry,
and the product Z1*Z is returned.
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
ILO and IHI mark the rows and columns of A which are to be
reduced.  It is assumed that A is already upper triangular
in rows and columns 1:ILO-1 and IHI+1:N.  ILO and IHI are
normally set by a previous call to SGGBAL; otherwise they
should be set to 1 and N respectively.
1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
On entry, the N-by-N general matrix to be reduced.
On exit, the upper triangle and the first subdiagonal of A
are overwritten with the upper Hessenberg matrix H, and the
rest is set to zero.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
On entry, the N-by-N upper triangular matrix B.
On exit, the upper triangular matrix T = Q**T B Z.  The
elements below the diagonal are set to zero.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
Q       (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
On entry, if COMPQ = \(aqV\(aq, the orthogonal matrix Q1,
typically from the QR factorization of B.
On exit, if COMPQ=\(aqI\(aq, the orthogonal matrix Q, and if
COMPQ = \(aqV\(aq, the product Q1*Q.
Not referenced if COMPQ=\(aqN\(aq.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.
LDQ >= N if COMPQ=\(aqV\(aq or \(aqI\(aq; LDQ >= 1 otherwise.
.TP 8
Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
On entry, if COMPZ = \(aqV\(aq, the orthogonal matrix Z1.
On exit, if COMPZ=\(aqI\(aq, the orthogonal matrix Z, and if
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
by Golub and Van Loan (Johns Hopkins Press.)
.br

