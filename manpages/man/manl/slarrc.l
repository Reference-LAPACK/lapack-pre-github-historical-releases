.TH  SLARRC 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLARRC - number of eigenvalues of the symmetric tridiagonal matrix T that are in the interval (VL,VU] if JOBT = \(aqT\(aq, and of L D L^T if JOBT = \(aqL\(aq
.SH SYNOPSIS
.TP 19
SUBROUTINE SLARRC(
JOBT, N, VL, VU, D, E, PIVMIN,
EIGCNT, LCNT, RCNT, INFO )
.TP 19
.ti +4
CHARACTER
JOBT
.TP 19
.ti +4
INTEGER
EIGCNT, INFO, LCNT, N, RCNT
.TP 19
.ti +4
REAL
PIVMIN, VL, VU
.TP 19
.ti +4
REAL
D( * ), E( * )
.SH PURPOSE
Find the number of eigenvalues of the symmetric tridiagonal matrix T
that are in the interval (VL,VU] if JOBT = \(aqT\(aq, and of L D L^T
if JOBT = \(aqL\(aq.

.SH ARGUMENTS
.TP 8
JOBT    (input) CHARACTER*1
= \(aqT\(aq:  Compute Sturm count for matrix T.
.br
= \(aqL\(aq:  Compute Sturm count for matrix L D L^T.
.TP 8
N       (input) INTEGER
The order of the matrix. N > 0.
.TP 8
VL      (input) DOUBLE PRECISION
VU      (input) DOUBLE PRECISION
The lower and upper bounds for the eigenvalues.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
JOBT = \(aqT\(aq: The N diagonal elements of the tridiagonal matrix T.
.br
JOBT = \(aqL\(aq: The N diagonal elements of the diagonal matrix D.
.TP 8
E       (input) DOUBLE PRECISION array, dimension (N)
.br
JOBT = \(aqT\(aq: The N-1 offdiagonal elements of the matrix T.
.br
JOBT = \(aqL\(aq: The N-1 offdiagonal elements of the matrix L.
.TP 8
PIVMIN  (input) DOUBLE PRECISION
The minimum pivot in the Sturm sequence for T.
.TP 8
EIGCNT  (output) INTEGER
The number of eigenvalues of the symmetric tridiagonal matrix T
that are in the interval (VL,VU]
.TP 8
LCNT    (output) INTEGER
RCNT    (output) INTEGER
The left and right negcounts of the interval.
.TP 8
INFO    (output) INTEGER
.SH FURTHER DETAILS
Based on contributions by
.br
   Beresford Parlett, University of California, Berkeley, USA
   Jim Demmel, University of California, Berkeley, USA
.br
   Inderjit Dhillon, University of Texas, Austin, USA
.br
   Osni Marques, LBNL/NERSC, USA
.br
   Christof Voemel, University of California, Berkeley, USA

