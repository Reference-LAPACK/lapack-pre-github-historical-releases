.TH  DLAGS2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAGS2 - 2-by-2 orthogonal matrices U, V and Q, such that if ( UPPER ) then   U\(aq*A*Q = U\(aq*( A1 A2 )*Q = ( x 0 )  ( 0 A3 ) ( x x ) and  V\(aq*B*Q = V\(aq*( B1 B2 )*Q = ( x 0 )  ( 0 B3 ) ( x x )  or if ( .NOT.UPPER ) then   U\(aq*A*Q = U\(aq*( A1 0 )*Q = ( x x )  ( A2 A3 ) ( 0 x ) and  V\(aq*B*Q = V\(aq*( B1 0 )*Q = ( x x )  ( B2 B3 ) ( 0 x )  The rows of the transformed A and B are parallel, where   U = ( CSU SNU ), V = ( CSV SNV ), Q = ( CSQ SNQ )  ( -SNU CSU ) ( -SNV CSV ) ( -SNQ CSQ )  Z\(aq denotes the transpose of Z
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAGS2(
UPPER, A1, A2, A3, B1, B2, B3, CSU, SNU, CSV,
SNV, CSQ, SNQ )
.TP 19
.ti +4
LOGICAL
UPPER
.TP 19
.ti +4
DOUBLE
PRECISION A1, A2, A3, B1, B2, B3, CSQ, CSU, CSV, SNQ,
SNU, SNV
.SH PURPOSE
DLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such
that if ( UPPER ) then


.SH ARGUMENTS
.TP 8
UPPER   (input) LOGICAL
= .TRUE.: the input matrices A and B are upper triangular.
.br
= .FALSE.: the input matrices A and B are lower triangular.
.TP 8
A1      (input) DOUBLE PRECISION
A2      (input) DOUBLE PRECISION
A3      (input) DOUBLE PRECISION
On entry, A1, A2 and A3 are elements of the input 2-by-2
upper (lower) triangular matrix A.
.TP 8
B1      (input) DOUBLE PRECISION
B2      (input) DOUBLE PRECISION
B3      (input) DOUBLE PRECISION
On entry, B1, B2 and B3 are elements of the input 2-by-2
upper (lower) triangular matrix B.
.TP 8
CSU     (output) DOUBLE PRECISION
SNU     (output) DOUBLE PRECISION
The desired orthogonal matrix U.
.TP 8
CSV     (output) DOUBLE PRECISION
SNV     (output) DOUBLE PRECISION
The desired orthogonal matrix V.
.TP 8
CSQ     (output) DOUBLE PRECISION
SNQ     (output) DOUBLE PRECISION
The desired orthogonal matrix Q.
