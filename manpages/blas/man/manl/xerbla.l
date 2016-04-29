.TH  XERBLA 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
XERBLA - an error handler for the LAPACK routines
.SH SYNOPSIS
.TP 31
SUBROUTINE XERBLA(SRNAME,INFO)
.TP 31
.ti +4
INTEGER
INFO
.TP 31
.ti +4
CHARACTER*6
SRNAME
.SH PURPOSE
XERBLA  is an error handler for the LAPACK routines.
It is called by an LAPACK routine if an input parameter has an
invalid value.  A message is printed and execution stops.

Installers may consider modifying the STOP statement in order to
call system-specific exception-handling facilities.
.br

.SH ARGUMENTS
.TP 8
SRNAME  (input) CHARACTER*6
The name of the routine which called XERBLA.
.TP 8
INFO    (input) INTEGER
The position of the invalid parameter in the parameter list
of the calling routine.

End of XERBLA
