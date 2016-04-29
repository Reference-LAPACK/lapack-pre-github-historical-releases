.TH  DLAE2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAE2 - the eigenvalues of a 2-by-2 symmetric matrix  [ A B ]  [ B C ]
.SH SYNOPSIS
.TP 18
SUBROUTINE DLAE2(
A, B, C, RT1, RT2 )
.TP 18
.ti +4
DOUBLE
PRECISION A, B, C, RT1, RT2
.SH PURPOSE
DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
   [  A   B  ]
   [  B   C  ].
On return, RT1 is the eigenvalue of larger absolute value, and RT2
is the eigenvalue of smaller absolute value.
.br

.SH ARGUMENTS
.TP 8
A       (input) DOUBLE PRECISION
The (1,1) element of the 2-by-2 matrix.
.TP 8
B       (input) DOUBLE PRECISION
The (1,2) and (2,1) elements of the 2-by-2 matrix.
.TP 8
C       (input) DOUBLE PRECISION
The (2,2) element of the 2-by-2 matrix.
.TP 8
RT1     (output) DOUBLE PRECISION
The eigenvalue of larger absolute value.
.TP 8
RT2     (output) DOUBLE PRECISION
The eigenvalue of smaller absolute value.
.SH FURTHER DETAILS
RT1 is accurate to a few ulps barring over/underflow.
.br

RT2 may be inaccurate if there is massive cancellation in the
determinant A*C-B*B; higher precision or correctly rounded or
correctly truncated arithmetic would be needed to compute RT2
accurately in all cases.
.br

Overflow is possible only if RT1 is within a factor of 5 of overflow.
Underflow is harmless if the input data is 0 or exceeds
.br
   underflow_threshold / macheps.
.br

