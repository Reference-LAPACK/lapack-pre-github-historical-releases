.TH  DGGEV 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DGGEV - for a pair of N-by-N real nonsymmetric matrices (A,B)
.SH SYNOPSIS
.TP 18
SUBROUTINE DGGEV(
JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR, ALPHAI,
BETA, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBVL, JOBVR
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, LDVL, LDVR, LWORK, N
.TP 18
.ti +4
DOUBLE
PRECISION A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
B( LDB, * ), BETA( * ), VL( LDVL, * ),
VR( LDVR, * ), WORK( * )
.SH PURPOSE
DGGEV computes for a pair of N-by-N real nonsymmetric matrices (A,B)
the generalized eigenvalues, and optionally, the left and/or right
generalized eigenvectors.
.br

A generalized eigenvalue for a pair of matrices (A,B) is a scalar
lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
singular. It is usually represented as the pair (alpha,beta), as
there is a reasonable interpretation for beta=0, and even for both
being zero.
.br

The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
of (A,B) satisfies
.br

                 A * v(j) = lambda(j) * B * v(j).
.br

The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
of (A,B) satisfies
.br

                 u(j)**H * A  = lambda(j) * u(j)**H * B .

where u(j)**H is the conjugate-transpose of u(j).
.br


.SH ARGUMENTS
.TP 8
JOBVL   (input) CHARACTER*1
= \(aqN\(aq:  do not compute the left generalized eigenvectors;
.br
= \(aqV\(aq:  compute the left generalized eigenvectors.
.TP 8
JOBVR   (input) CHARACTER*1
.br
= \(aqN\(aq:  do not compute the right generalized eigenvectors;
.br
= \(aqV\(aq:  compute the right generalized eigenvectors.
.TP 8
N       (input) INTEGER
The order of the matrices A, B, VL, and VR.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
On entry, the matrix A in the pair (A,B).
On exit, A has been overwritten.
.TP 8
LDA     (input) INTEGER
The leading dimension of A.  LDA >= max(1,N).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
On entry, the matrix B in the pair (A,B).
On exit, B has been overwritten.
.TP 8
LDB     (input) INTEGER
The leading dimension of B.  LDB >= max(1,N).
.TP 8
ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
BETA    (output) DOUBLE PRECISION array, dimension (N)
On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
be the generalized eigenvalues.  If ALPHAI(j) is zero, then
the j-th eigenvalue is real; if positive, then the j-th and
(j+1)-st eigenvalues are a complex conjugate pair, with
ALPHAI(j+1) negative.

Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
may easily over- or underflow, and BETA(j) may even be zero.
Thus, the user should avoid naively computing the ratio
alpha/beta.  However, ALPHAR and ALPHAI will be always less
than and usually comparable with norm(A) in magnitude, and
BETA always less than and usually comparable with norm(B).
.TP 8
VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
If JOBVL = \(aqV\(aq, the left eigenvectors u(j) are stored one
after another in the columns of VL, in the same order as
their eigenvalues. If the j-th eigenvalue is real, then
u(j) = VL(:,j), the j-th column of VL. If the j-th and
(j+1)-th eigenvalues form a complex conjugate pair, then
u(j) = VL(:,j)+i*VL(:,j+1) and u(j+1) = VL(:,j)-i*VL(:,j+1).
Each eigenvector is scaled so the largest component has
abs(real part)+abs(imag. part)=1.
Not referenced if JOBVL = \(aqN\(aq.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the matrix VL. LDVL >= 1, and
if JOBVL = \(aqV\(aq, LDVL >= N.
.TP 8
VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
If JOBVR = \(aqV\(aq, the right eigenvectors v(j) are stored one
after another in the columns of VR, in the same order as
their eigenvalues. If the j-th eigenvalue is real, then
v(j) = VR(:,j), the j-th column of VR. If the j-th and
(j+1)-th eigenvalues form a complex conjugate pair, then
v(j) = VR(:,j)+i*VR(:,j+1) and v(j+1) = VR(:,j)-i*VR(:,j+1).
Each eigenvector is scaled so the largest component has
abs(real part)+abs(imag. part)=1.
Not referenced if JOBVR = \(aqN\(aq.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the matrix VR. LDVR >= 1, and
if JOBVR = \(aqV\(aq, LDVR >= N.
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,8*N).
For good performance, LWORK must generally be larger.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
= 1,...,N:
The QZ iteration failed.  No eigenvectors have been
calculated, but ALPHAR(j), ALPHAI(j), and BETA(j)
should be correct for j=INFO+1,...,N.
> N:  =N+1: other than QZ iteration failed in DHGEQZ.
.br
=N+2: error return from DTGEVC.
