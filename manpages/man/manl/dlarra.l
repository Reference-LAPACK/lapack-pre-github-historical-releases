.TH  DLARRA 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLARRA - splitting points with threshold SPLTOL
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARRA(
N, D, E, E2, SPLTOL, TNRM,
NSPLIT, ISPLIT, INFO )
.TP 19
.ti +4
IMPLICIT
NONE
.TP 19
.ti +4
INTEGER
INFO, N, NSPLIT
.TP 19
.ti +4
DOUBLE
PRECISION SPLTOL, TNRM
.TP 19
.ti +4
INTEGER
ISPLIT( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), E2( * )
.SH PURPOSE
Compute the splitting points with threshold SPLTOL.
DLARRA sets any "small" off-diagonal elements to zero.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix. N > 0.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
On entry, the N diagonal elements of the tridiagonal
matrix T.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the first (N-1) entries contain the subdiagonal
elements of the tridiagonal matrix T; E(N) need not be set.
On exit, the entries E( ISPLIT( I ) ), 1 <= I <= NSPLIT,
are set to zero, the other entries of E are untouched.
.TP 8
E2      (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the first (N-1) entries contain the SQUARES of the
subdiagonal elements of the tridiagonal matrix T;
E2(N) need not be set.
On exit, the entries E2( ISPLIT( I ) ),
1 <= I <= NSPLIT, have been set to zero

SPLTOL (input) DOUBLE PRECISION
The threshold for splitting. Two criteria can be used:
.br
SPLTOL<0 : criterion based on absolute off-diagonal value
.br
SPLTOL>0 : criterion that preserves relative accuracy

TNRM (input) DOUBLE PRECISION
The norm of the matrix.
.TP 8
NSPLIT  (output) INTEGER
The number of blocks T splits into. 1 <= NSPLIT <= N.
.TP 8
ISPLIT  (output) INTEGER array, dimension (N)
The splitting points, at which T breaks up into blocks.
The first block consists of rows/columns 1 to ISPLIT(1),
the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
etc., and the NSPLIT-th consists of rows/columns
ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
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

