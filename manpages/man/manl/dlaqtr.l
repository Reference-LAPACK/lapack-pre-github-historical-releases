.TH  DLAQTR 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAQTR - the real quasi-triangular system   op(T)*p = scale*c, if LREAL = .TRUE
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAQTR(
LTRAN, LREAL, N, T, LDT, B, W, SCALE, X, WORK,
INFO )
.TP 19
.ti +4
LOGICAL
LREAL, LTRAN
.TP 19
.ti +4
INTEGER
INFO, LDT, N
.TP 19
.ti +4
DOUBLE
PRECISION SCALE, W
.TP 19
.ti +4
DOUBLE
PRECISION B( * ), T( LDT, * ), WORK( * ), X( * )
.SH PURPOSE
DLAQTR solves the real quasi-triangular system

or the complex quasi-triangular systems
.br

           op(T + iB)*(p+iq) = scale*(c+id),  if LREAL = .FALSE.

in real arithmetic, where T is upper quasi-triangular.
.br
If LREAL = .FALSE., then the first diagonal block of T must be
1 by 1, B is the specially structured matrix
.br

               B = [ b(1) b(2) ... b(n) ]
.br
                   [       w            ]
.br
                   [           w        ]
.br
                   [              .     ]
.br
                   [                 w  ]
.br

op(A) = A or A\(aq, A\(aq denotes the conjugate transpose of
.br
matrix A.
.br

On input, X = [ c ].  On output, X = [ p ].
.br
              [ d ]                  [ q ]
.br

This subroutine is designed for the condition number estimation
in routine DTRSNA.
.br

.SH ARGUMENTS
.TP 8
LTRAN   (input) LOGICAL
On entry, LTRAN specifies the option of conjugate transpose:
= .FALSE.,    op(T+i*B) = T+i*B,
= .TRUE.,     op(T+i*B) = (T+i*B)\(aq.
.TP 8
LREAL   (input) LOGICAL
On entry, LREAL specifies the input matrix structure:
= .FALSE.,    the input is complex
= .TRUE.,     the input is real
.TP 8
N       (input) INTEGER
On entry, N specifies the order of T+i*B. N >= 0.
.TP 8
T       (input) DOUBLE PRECISION array, dimension (LDT,N)
On entry, T contains a matrix in Schur canonical form.
If LREAL = .FALSE., then the first diagonal block of T mu
be 1 by 1.
.TP 8
LDT     (input) INTEGER
The leading dimension of the matrix T. LDT >= max(1,N).
.TP 8
B       (input) DOUBLE PRECISION array, dimension (N)
On entry, B contains the elements to form the matrix
B as described above.
If LREAL = .TRUE., B is not referenced.
.TP 8
W       (input) DOUBLE PRECISION
On entry, W is the diagonal element of the matrix B.
If LREAL = .TRUE., W is not referenced.
.TP 8
SCALE   (output) DOUBLE PRECISION
On exit, SCALE is the scale factor.
.TP 8
X       (input/output) DOUBLE PRECISION array, dimension (2*N)
On entry, X contains the right hand side of the system.
On exit, X is overwritten by the solution.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
INFO    (output) INTEGER
On exit, INFO is set to
0: successful exit.
.br
1: the some diagonal 1 by 1 block has been perturbed by
a small number SMIN to keep nonsingularity.
2: the some diagonal 2 by 2 block has been perturbed by
a small number in DLALN2 to keep nonsingularity.
NOTE: In the interests of speed, this routine does not
check the inputs for errors.
