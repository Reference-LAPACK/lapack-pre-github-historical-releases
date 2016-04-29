.TH  SLAEV2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAEV2 - the eigendecomposition of a 2-by-2 symmetric matrix  [ A B ]  [ B C ]
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAEV2(
A, B, C, RT1, RT2, CS1, SN1 )
.TP 19
.ti +4
REAL
A, B, C, CS1, RT1, RT2, SN1
.SH PURPOSE
SLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix
   [  A   B  ]
   [  B   C  ].
On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
eigenvector for RT1, giving the decomposition
.br

   [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
.br
   [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].

.SH ARGUMENTS
.TP 8
A       (input) REAL
The (1,1) element of the 2-by-2 matrix.
.TP 8
B       (input) REAL
The (1,2) element and the conjugate of the (2,1) element of
the 2-by-2 matrix.
.TP 8
C       (input) REAL
The (2,2) element of the 2-by-2 matrix.
.TP 8
RT1     (output) REAL
The eigenvalue of larger absolute value.
.TP 8
RT2     (output) REAL
The eigenvalue of smaller absolute value.
.TP 8
CS1     (output) REAL
SN1     (output) REAL
The vector (CS1, SN1) is a unit right eigenvector for RT1.
.SH FURTHER DETAILS
RT1 is accurate to a few ulps barring over/underflow.
.br

RT2 may be inaccurate if there is massive cancellation in the
determinant A*C-B*B; higher precision or correctly rounded or
correctly truncated arithmetic would be needed to compute RT2
accurately in all cases.
.br

CS1 and SN1 are accurate to a few ulps barring over/underflow.

Overflow is possible only if RT1 is within a factor of 5 of overflow.
Underflow is harmless if the input data is 0 or exceeds
.br
   underflow_threshold / macheps.
.br

