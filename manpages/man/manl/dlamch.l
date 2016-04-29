.SH NAME
DLAMCH - double precision machine parameters
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DLAMCH( CMACH )
.TP 17
.ti +4
CHARACTER
CMACH
.SH PURPOSE
DLAMCH determines double precision machine parameters.

.SH ARGUMENTS
.TP 8
CMACH   (input) CHARACTER*1
Specifies the value to be returned by DLAMCH:
.br
= \(aqE\(aq or \(aqe\(aq,   DLAMCH := eps
.br
= \(aqS\(aq or \(aqs ,   DLAMCH := sfmin
.br
= \(aqB\(aq or \(aqb\(aq,   DLAMCH := base
.br
= \(aqP\(aq or \(aqp\(aq,   DLAMCH := eps*base
.br
= \(aqN\(aq or \(aqn\(aq,   DLAMCH := t
.br
= \(aqR\(aq or \(aqr\(aq,   DLAMCH := rnd
.br
= \(aqM\(aq or \(aqm\(aq,   DLAMCH := emin
.br
= \(aqU\(aq or \(aqu\(aq,   DLAMCH := rmin
.br
= \(aqL\(aq or \(aql\(aq,   DLAMCH := emax
.br
= \(aqO\(aq or \(aqo\(aq,   DLAMCH := rmax

where
.TP 6
eps   = relative machine precision
sfmin = safe minimum, such that 1/sfmin does not overflow
base  = base of the machine
prec  = eps*base
t     = number of (base) digits in the mantissa
rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
emin  = minimum exponent before (gradual) underflow
rmin  = underflow threshold - base**(emin-1)
emax  = largest exponent before overflow
rmax  = overflow threshold  - (base**emax)*(1-eps)
