.TH  SLANEG 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLANEG - the Sturm count, the number of negative pivots encountered while factoring tridiagonal T - sigma I = L D L^T
.SH SYNOPSIS
.TP 17
FUNCTION SLANEG(
N, D, LLD, SIGMA, PIVMIN, R )
.TP 17
.ti +4
IMPLICIT
NONE
.TP 17
.ti +4
INTEGER
SLANEG
.TP 17
.ti +4
INTEGER
N, R
.TP 17
.ti +4
REAL
PIVMIN, SIGMA
.TP 17
.ti +4
REAL
D( * ), LLD( * )
.SH PURPOSE
SLANEG computes the Sturm count, the number of negative pivots
encountered while factoring tridiagonal T - sigma I = L D L^T.
This implementation works directly on the factors without forming
the tridiagonal matrix T.  The Sturm count is also the number of
eigenvalues of T less than sigma.
.br

This routine is called from SLARRB.
.br

The current routine does not use the PIVMIN parameter but rather
requires IEEE-754 propagation of Infinities and NaNs.  This
routine also has no input range restrictions but does require
default exception handling such that x/0 produces Inf when x is
non-zero, and Inf/Inf produces NaN.  For more information, see:

  Marques, Riedy, and Voemel, "Benefits of IEEE-754 Features in
  Modern Symmetric Tridiagonal Eigensolvers," SIAM Journal on
  Scientific Computing, v28, n5, 2006.  DOI 10.1137/050641624
  (Tech report version in LAWN 172 with the same title.)
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix.
.TP 8
D       (input) REAL             array, dimension (N)
The N diagonal elements of the diagonal matrix D.
.TP 8
LLD     (input) REAL             array, dimension (N-1)
The (N-1) elements L(i)*L(i)*D(i).
.TP 8
SIGMA   (input) REAL            
Shift amount in T - sigma I = L D L^T.
.TP 8
PIVMIN  (input) REAL            
The minimum pivot in the Sturm sequence.  May be used
when zero pivots are encountered on non-IEEE-754
architectures.
.TP 8
R       (input) INTEGER
The twist index for the twisted factorization that is used
for the negcount.
.SH FURTHER DETAILS
Based on contributions by
.br
   Osni Marques, LBNL/NERSC, USA
.br
   Christof Voemel, University of California, Berkeley, USA
   Jason Riedy, University of California, Berkeley, USA
.br

