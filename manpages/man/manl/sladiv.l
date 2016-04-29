.TH  SLADIV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLADIV - complex division in real arithmetic   a + i*b  p + i*q = ---------  c + i*d  The algorithm is due to Robert L
.SH SYNOPSIS
.TP 19
SUBROUTINE SLADIV(
A, B, C, D, P, Q )
.TP 19
.ti +4
REAL
A, B, C, D, P, Q
.SH PURPOSE
SLADIV performs complex division in  real arithmetic
in D. Knuth, The art of Computer Programming, Vol.2, p.195

.SH ARGUMENTS
.TP 8
A       (input) REAL
B       (input) REAL
C       (input) REAL
D       (input) REAL
The scalars a, b, c, and d in the above expression.
.TP 8
P       (output) REAL
Q       (output) REAL
The scalars p and q in the above expression.
