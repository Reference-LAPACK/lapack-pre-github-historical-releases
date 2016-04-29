.TH  IEEECK 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
IEEECK - called from the ILAENV to verify that Infinity and possibly NaN arithmetic is safe (i.e
.SH SYNOPSIS
.TP 17
INTEGER FUNCTION
IEEECK( ISPEC, ZERO, ONE )
.TP 17
.ti +4
INTEGER
ISPEC
.TP 17
.ti +4
REAL
ONE, ZERO
.SH PURPOSE
IEEECK is called from the ILAENV to verify that Infinity and
possibly NaN arithmetic is safe (i.e. will not trap).

.SH ARGUMENTS
.TP 8
ISPEC   (input) INTEGER
Specifies whether to test just for inifinity arithmetic
or whether to test for infinity and NaN arithmetic.
= 0: Verify infinity arithmetic only.
.br
= 1: Verify infinity and NaN arithmetic.
.TP 8
ZERO    (input) REAL
Must contain the value 0.0
This is passed to prevent the compiler from optimizing
away this code.
.TP 8
ONE     (input) REAL
Must contain the value 1.0
This is passed to prevent the compiler from optimizing
away this code.

RETURN VALUE:  INTEGER
= 0:  Arithmetic failed to produce the correct answers
.br
= 1:  Arithmetic produced the correct answers

.. Local Scalars ..

..
.. Executable Statements ..

Return if we were only asked to check infinity arithmetic
