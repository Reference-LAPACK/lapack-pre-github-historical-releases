.TH  SLARRF 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLARRF - initial representation L D L^T and its cluster of close eigenvalues (in a relative measure), W( CLSTRT ), W( CLSTRT+1 ), ..
.SH SYNOPSIS
.TP 19
SUBROUTINE SLARRF(
N, D, L, LD, CLSTRT, CLEND,
W, WGAP, WERR,
SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA,
DPLUS, LPLUS, WORK, INFO )
.TP 19
.ti +4
INTEGER
CLSTRT, CLEND, INFO, N
.TP 19
.ti +4
REAL
CLGAPL, CLGAPR, PIVMIN, SIGMA, SPDIAM
.TP 19
.ti +4
REAL
D( * ), DPLUS( * ), L( * ), LD( * ),
LPLUS( * ), W( * ), WGAP( * ), WERR( * ), WORK( * )
.SH PURPOSE
Given the initial representation L D L^T and its cluster of close
eigenvalues (in a relative measure), W( CLSTRT ), W( CLSTRT+1 ), ...
W( CLEND ), SLARRF finds a new relatively robust representation
L D L^T - SIGMA I = L(+) D(+) L(+)^T such that at least one of the
eigenvalues of L(+) D(+) L(+)^T is relatively isolated.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix (subblock, if the matrix splitted).
.TP 8
D       (input) REAL             array, dimension (N)
The N diagonal elements of the diagonal matrix D.
.TP 8
L       (input) REAL             array, dimension (N-1)
The (N-1) subdiagonal elements of the unit bidiagonal
matrix L.
.TP 8
LD      (input) REAL             array, dimension (N-1)
The (N-1) elements L(i)*D(i).
.TP 8
CLSTRT  (input) INTEGER
The index of the first eigenvalue in the cluster.
.TP 8
CLEND   (input) INTEGER
The index of the last eigenvalue in the cluster.
.TP 8
W       (input) REAL             array, dimension >=  (CLEND-CLSTRT+1)
The eigenvalue APPROXIMATIONS of L D L^T in ascending order.
W( CLSTRT ) through W( CLEND ) form the cluster of relatively
close eigenalues.
.TP 8
WGAP    (input/output) REAL             array, dimension >=  (CLEND-CLSTRT+1)
The separation from the right neighbor eigenvalue in W.
.TP 8
WERR    (input) REAL             array, dimension >=  (CLEND-CLSTRT+1)
WERR contain the semiwidth of the uncertainty
interval of the corresponding eigenvalue APPROXIMATION in W

SPDIAM (input) estimate of the spectral diameter obtained from the
Gerschgorin intervals

CLGAPL, CLGAPR (input) absolute gap on each end of the cluster.
Set by the calling routine to protect against shifts too close
to eigenvalues outside the cluster.
.TP 8
PIVMIN  (input) DOUBLE PRECISION
The minimum pivot allowed in the Sturm sequence.
.TP 8
SIGMA   (output) REAL            
The shift used to form L(+) D(+) L(+)^T.
.TP 8
DPLUS   (output) REAL             array, dimension (N)
The N diagonal elements of the diagonal matrix D(+).
.TP 8
LPLUS   (output) REAL             array, dimension (N-1)
The first (N-1) elements of LPLUS contain the subdiagonal
elements of the unit bidiagonal matrix L(+).
.TP 8
WORK    (workspace) REAL             array, dimension (2*N)
Workspace.
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

