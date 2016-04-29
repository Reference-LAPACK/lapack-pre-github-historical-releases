.TH  DLADIV 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLADIV - complex division in real arithmetic   a + i*b  p + i*q = ---------  c + i*d  The algorithm is due to Robert L
.SH SYNOPSIS
.TP 19
SUBROUTINE DLADIV(
A, B, C, D, P, Q )
.TP 19
.ti +4
DOUBLE
PRECISION A, B, C, D, P, Q
.SH PURPOSE
DLADIV performs complex division in  real arithmetic
in D. Knuth, The art of Computer Programming, Vol.2, p.195

.SH ARGUMENTS
.TP 8
A       (input) DOUBLE PRECISION
B       (input) DOUBLE PRECISION
C       (input) DOUBLE PRECISION
D       (input) DOUBLE PRECISION
The scalars a, b, c, and d in the above expression.
.TP 8
P       (output) DOUBLE PRECISION
Q       (output) DOUBLE PRECISION
The scalars p and q in the above expression.
