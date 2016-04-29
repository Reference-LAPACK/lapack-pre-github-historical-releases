.TH  DLARRR 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARRR - to decide whether the symmetric tridiagonal matrix T warrants expensive computations which guarantee high relative accuracy in the eigenvalues
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARRR(
N, D, E, INFO )
.TP 19
.ti +4
INTEGER
N, INFO
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * )
.SH PURPOSE
Perform tests to decide whether the symmetric tridiagonal matrix T
warrants expensive computations which guarantee high relative accuracy
in the eigenvalues.

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix. N > 0.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The N diagonal elements of the tridiagonal matrix T.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the first (N-1) entries contain the subdiagonal
elements of the tridiagonal matrix T; E(N) is set to ZERO.
.TP 8
INFO    (output) INTEGER
INFO = 0(default) : the matrix warrants computations preserving
relative accuracy.
INFO = 1          : the matrix warrants computations guaranteeing
only absolute accuracy.
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

