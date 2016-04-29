.TH  CLAGS2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLAGS2 - 2-by-2 unitary matrices U, V and Q, such that if ( UPPER ) then   U\(aq*A*Q = U\(aq*( A1 A2 )*Q = ( x 0 )  ( 0 A3 ) ( x x ) and  V\(aq*B*Q = V\(aq*( B1 B2 )*Q = ( x 0 )  ( 0 B3 ) ( x x )  or if ( .NOT.UPPER ) then   U\(aq*A*Q = U\(aq*( A1 0 )*Q = ( x x )  ( A2 A3 ) ( 0 x ) and  V\(aq*B*Q = V\(aq*( B1 0 )*Q = ( x x )  ( B2 B3 ) ( 0 x ) where   U = ( CSU SNU ), V = ( CSV SNV ),
.SH SYNOPSIS
.TP 19
SUBROUTINE CLAGS2(
UPPER, A1, A2, A3, B1, B2, B3, CSU, SNU, CSV,
SNV, CSQ, SNQ )
.TP 19
.ti +4
LOGICAL
UPPER
.TP 19
.ti +4
REAL
A1, A3, B1, B3, CSQ, CSU, CSV
.TP 19
.ti +4
COMPLEX
A2, B2, SNQ, SNU, SNV
.SH PURPOSE
CLAGS2 computes 2-by-2 unitary matrices U, V and Q, such
that if ( UPPER ) then
      ( -CONJG(SNU)  CSU )      ( -CONJG(SNV) CSV )
.br

  Q = (     CSQ      SNQ )
.br
      ( -CONJG(SNQ)  CSQ )
.br

Z\(aq denotes the conjugate transpose of Z.
.br

The rows of the transformed A and B are parallel. Moreover, if the
input 2-by-2 matrix A is not zero, then the transformed (1,1) entry
of A is not zero. If the input matrices A and B are both not zero,
then the transformed (2,2) element of B is not zero, except when the
first rows of input A and B are parallel and the second rows are
zero.
.br

.SH ARGUMENTS
.TP 8
UPPER   (input) LOGICAL
= .TRUE.: the input matrices A and B are upper triangular.
.br
= .FALSE.: the input matrices A and B are lower triangular.
.TP 8
A1      (input) REAL
A2      (input) COMPLEX
A3      (input) REAL
On entry, A1, A2 and A3 are elements of the input 2-by-2
upper (lower) triangular matrix A.
.TP 8
B1      (input) REAL
B2      (input) COMPLEX
B3      (input) REAL
On entry, B1, B2 and B3 are elements of the input 2-by-2
upper (lower) triangular matrix B.
.TP 8
CSU     (output) REAL
SNU     (output) COMPLEX
The desired unitary matrix U.
.TP 8
CSV     (output) REAL
SNV     (output) COMPLEX
The desired unitary matrix V.
.TP 8
CSQ     (output) REAL
SNQ     (output) COMPLEX
The desired unitary matrix Q.
