.TH  LSAME 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
LSAME - .TRUE
.SH SYNOPSIS
.TP 17
LOGICAL FUNCTION
LSAME( CA, CB )
.TP 17
.ti +4
CHARACTER
CA, CB
.SH PURPOSE
LSAME returns .TRUE. if CA is the same letter as CB regardless of
case.
.br

.SH ARGUMENTS
.TP 8
CA      (input) CHARACTER*1
CB      (input) CHARACTER*1
CA and CB specify the single characters to be compared.
