.TH  SECOND_NONE 1 "February 2007" " LAPACK auxiliary routine (version 3.1.1) " " LAPACK auxiliary routine (version 3.1.1) " 
.SH NAME
SECOND_NONE - nothing instead of returning the user time for a process in seconds
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SECOND( )
.SH PURPOSE
SECOND returns nothing instead of returning the user time for a process in seconds.
If you are using that routine, it means that neither EXTERNAL ETIME,
EXTERNAL ETIME_, INTERNAL ETIME, INTERNAL CPU_TIME is available  on
your machine.
.br

