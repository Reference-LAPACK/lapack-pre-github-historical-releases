.TH  SNRM2 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
SNRM2 - the euclidean norm of a vector via the function name, so that   SNRM2 := sqrt( x\(aq*x )
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SNRM2(N,X,INCX)
.TP 14
.ti +4
INTEGER
INCX,N
.TP 14
.ti +4
REAL
X(*)
.SH PURPOSE
SNRM2 returns the euclidean norm of a vector via the function
name, so that

.SH FURTHER DETAILS
-- This version written on 25-October-1982.
.br
   Modified on 14-October-1993 to inline the call to SLASSQ.
   Sven Hammarling, Nag Ltd.
.br


