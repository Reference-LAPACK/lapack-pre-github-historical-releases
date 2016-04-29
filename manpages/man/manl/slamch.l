.SH NAME
SLAMCH - single precision machine parameters
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SLAMCH( CMACH )
.TP 14
.ti +4
CHARACTER
CMACH
.SH PURPOSE
SLAMCH determines single precision machine parameters.

.SH ARGUMENTS
.TP 8
CMACH   (input) CHARACTER*1
Specifies the value to be returned by SLAMCH:
.br
= \(aqE\(aq or \(aqe\(aq,   SLAMCH := eps
.br
= \(aqS\(aq or \(aqs ,   SLAMCH := sfmin
.br
= \(aqB\(aq or \(aqb\(aq,   SLAMCH := base
.br
= \(aqP\(aq or \(aqp\(aq,   SLAMCH := eps*base
.br
= \(aqN\(aq or \(aqn\(aq,   SLAMCH := t
.br
= \(aqR\(aq or \(aqr\(aq,   SLAMCH := rnd
.br
= \(aqM\(aq or \(aqm\(aq,   SLAMCH := emin
.br
= \(aqU\(aq or \(aqu\(aq,   SLAMCH := rmin
.br
= \(aqL\(aq or \(aql\(aq,   SLAMCH := emax
.br
= \(aqO\(aq or \(aqo\(aq,   SLAMCH := rmax

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
