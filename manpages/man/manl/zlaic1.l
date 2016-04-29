.TH  ZLAIC1 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLAIC1 - one step of incremental condition estimation in its simplest version
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAIC1(
JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
.TP 19
.ti +4
INTEGER
J, JOB
.TP 19
.ti +4
DOUBLE
PRECISION SEST, SESTPR
.TP 19
.ti +4
COMPLEX*16
C, GAMMA, S
.TP 19
.ti +4
COMPLEX*16
W( J ), X( J )
.SH PURPOSE
ZLAIC1 applies one step of incremental condition estimation in
its simplest version:

Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
lower triangular matrix L, such that
.br
         twonorm(L*x) = sest
.br
Then ZLAIC1 computes sestpr, s, c such that
.br
the vector
.br
                [ s*x ]
.br
         xhat = [  c  ]
.br
is an approximate singular vector of
.br
                [ L     0  ]
.br
         Lhat = [ w\(aq gamma ]
.br
in the sense that
.br
         twonorm(Lhat*xhat) = sestpr.
.br

Depending on JOB, an estimate for the largest or smallest singular
value is computed.
.br

Note that [s c]\(aq and sestpr**2 is an eigenpair of the system

    diag(sest*sest, 0) + [alpha  gamma] * [ conjg(alpha) ]
                                          [ conjg(gamma) ]

where  alpha =  conjg(x)\(aq*w.
.br

.SH ARGUMENTS
.TP 8
JOB     (input) INTEGER
= 1: an estimate for the largest singular value is computed.
.br
= 2: an estimate for the smallest singular value is computed.
.TP 8
J       (input) INTEGER
Length of X and W
.TP 8
X       (input) COMPLEX*16 array, dimension (J)
The j-vector x.
.TP 8
SEST    (input) DOUBLE PRECISION
Estimated singular value of j by j matrix L
.TP 8
W       (input) COMPLEX*16 array, dimension (J)
The j-vector w.
.TP 8
GAMMA   (input) COMPLEX*16
The diagonal element gamma.
.TP 8
SESTPR  (output) DOUBLE PRECISION
Estimated singular value of (j+1) by (j+1) matrix Lhat.
.TP 8
S       (output) COMPLEX*16
Sine needed in forming xhat.
.TP 8
C       (output) COMPLEX*16
Cosine needed in forming xhat.
