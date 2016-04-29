.TH  DLANV2 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
DLANV2 - the Schur factorization of a real 2-by-2 nonsymmetric matrix in standard form
.SH SYNOPSIS
.TP 19
SUBROUTINE DLANV2(
A, B, C, D, RT1R, RT1I, RT2R, RT2I, CS, SN )
.TP 19
.ti +4
DOUBLE
PRECISION A, B, C, CS, D, RT1I, RT1R, RT2I, RT2R, SN
.SH PURPOSE
DLANV2 computes the Schur factorization of a real 2-by-2 nonsymmetric
matrix in standard form:

     [ A  B ] = [ CS -SN ] [ AA  BB ] [ CS  SN ]
.br
     [ C  D ]   [ SN  CS ] [ CC  DD ] [-SN  CS ]
.br

where either
.br
1) CC = 0 so that AA and DD are real eigenvalues of the matrix, or
2) AA = DD and BB*CC < 0, so that AA + or - sqrt(BB*CC) are complex
conjugate eigenvalues.
.br

.SH ARGUMENTS
.TP 8
A       (input/output) DOUBLE PRECISION
B       (input/output) DOUBLE PRECISION
C       (input/output) DOUBLE PRECISION
D       (input/output) DOUBLE PRECISION
On entry, the elements of the input matrix.
On exit, they are overwritten by the elements of the
standardised Schur form.
.TP 8
RT1R    (output) DOUBLE PRECISION
RT1I    (output) DOUBLE PRECISION
RT2R    (output) DOUBLE PRECISION
RT2I    (output) DOUBLE PRECISION
The real and imaginary parts of the eigenvalues. If the
eigenvalues are a complex conjugate pair, RT1I > 0.
.TP 8
CS      (output) DOUBLE PRECISION
SN      (output) DOUBLE PRECISION
Parameters of the rotation matrix.
.SH FURTHER DETAILS
Modified by V. Sima, Research Institute for Informatics, Bucharest,
Romania, to reduce the risk of cancellation errors,
.br
when computing real eigenvalues, and to ensure, if possible, that
abs(RT1R) >= abs(RT2R).
.br

