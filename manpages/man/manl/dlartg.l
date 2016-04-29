.TH  DLARTG 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARTG - a plane rotation so that   [ CS SN ] 
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARTG(
F, G, CS, SN, R )
.TP 19
.ti +4
DOUBLE
PRECISION CS, F, G, R, SN
.SH PURPOSE
DLARTG generate a plane rotation so that
   [ -SN  CS  ]     [ G ]     [ 0 ]
.br

This is a slower, more accurate version of the BLAS1 routine DROTG,
with the following other differences:
.br
   F and G are unchanged on return.
.br
   If G=0, then CS=1 and SN=0.
.br
   If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
      floating point operations (saves work in DBDSQR when
      there are zeros on the diagonal).
.br

If F exceeds G in magnitude, CS will be positive.
.br

.SH ARGUMENTS
.TP 8
F       (input) DOUBLE PRECISION
The first component of vector to be rotated.
.TP 8
G       (input) DOUBLE PRECISION
The second component of vector to be rotated.
.TP 8
CS      (output) DOUBLE PRECISION
The cosine of the rotation.
.TP 8
SN      (output) DOUBLE PRECISION
The sine of the rotation.
.TP 8
R       (output) DOUBLE PRECISION
The nonzero component of the rotated vector.

This version has a few statements commented out for thread safety
(machine parameters are computed on each entry). 10 feb 03, SJH.
