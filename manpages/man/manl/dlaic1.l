.TH  DLAIC1 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAIC1 - one step of incremental condition estimation in its simplest version
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAIC1(
JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
.TP 19
.ti +4
INTEGER
J, JOB
.TP 19
.ti +4
DOUBLE
PRECISION C, GAMMA, S, SEST, SESTPR
.TP 19
.ti +4
DOUBLE
PRECISION W( J ), X( J )
.SH PURPOSE
DLAIC1 applies one step of incremental condition estimation in
its simplest version:

Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
lower triangular matrix L, such that
.br
         twonorm(L*x) = sest
.br
Then DLAIC1 computes sestpr, s, c such that
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

    diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
.br
                                          [ gamma ]
.br

where  alpha =  x\(aq*w.
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
X       (input) DOUBLE PRECISION array, dimension (J)
The j-vector x.
.TP 8
SEST    (input) DOUBLE PRECISION
Estimated singular value of j by j matrix L
.TP 8
W       (input) DOUBLE PRECISION array, dimension (J)
The j-vector w.
.TP 8
GAMMA   (input) DOUBLE PRECISION
The diagonal element gamma.
.TP 8
SESTPR  (output) DOUBLE PRECISION
Estimated singular value of (j+1) by (j+1) matrix Lhat.
.TP 8
S       (output) DOUBLE PRECISION
Sine needed in forming xhat.
.TP 8
C       (output) DOUBLE PRECISION
Cosine needed in forming xhat.
