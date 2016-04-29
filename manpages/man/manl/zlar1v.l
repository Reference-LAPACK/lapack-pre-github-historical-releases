.TH  ZLAR1V 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLAR1V - the (scaled) r-th column of the inverse of the sumbmatrix in rows B1 through BN of the tridiagonal matrix L D L^T - sigma I
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAR1V(
N, B1, BN, LAMBDA, D, L, LD, LLD,
PIVMIN, GAPTOL, Z, WANTNC, NEGCNT, ZTZ, MINGMA,
R, ISUPPZ, NRMINV, RESID, RQCORR, WORK )
.TP 19
.ti +4
LOGICAL
WANTNC
.TP 19
.ti +4
INTEGER
B1, BN, N, NEGCNT, R
.TP 19
.ti +4
DOUBLE
PRECISION GAPTOL, LAMBDA, MINGMA, NRMINV, PIVMIN, RESID,
RQCORR, ZTZ
.TP 19
.ti +4
INTEGER
ISUPPZ( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), L( * ), LD( * ), LLD( * ),
WORK( * )
.TP 19
.ti +4
COMPLEX*16
Z( * )
.SH PURPOSE
ZLAR1V computes the (scaled) r-th column of the inverse of
the sumbmatrix in rows B1 through BN of the tridiagonal matrix
L D L^T - sigma I. When sigma is close to an eigenvalue, the
computed vector is an accurate eigenvector. Usually, r corresponds
to the index where the eigenvector is largest in magnitude.
The following steps accomplish this computation :
.br
(a) Stationary qd transform,  L D L^T - sigma I = L(+) D(+) L(+)^T,
(b) Progressive qd transform, L D L^T - sigma I = U(-) D(-) U(-)^T,
(c) Computation of the diagonal elements of the inverse of
    L D L^T - sigma I by combining the above transforms, and choosing
    r as the index where the diagonal of the inverse is (one of the)
    largest in magnitude.
.br
(d) Computation of the (scaled) r-th column of the inverse using the
    twisted factorization obtained by combining the top part of the
    the stationary and the bottom part of the progressive transform.

.SH ARGUMENTS
.TP 9
N        (input) INTEGER
The order of the matrix L D L^T.
.TP 9
B1       (input) INTEGER
First index of the submatrix of L D L^T.
.TP 9
BN       (input) INTEGER
Last index of the submatrix of L D L^T.
.TP 10
LAMBDA    (input) DOUBLE PRECISION
The shift. In order to compute an accurate eigenvector,
LAMBDA should be a good approximation to an eigenvalue
of L D L^T.
.TP 9
L        (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) subdiagonal elements of the unit bidiagonal matrix
L, in elements 1 to N-1.
.TP 9
D        (input) DOUBLE PRECISION array, dimension (N)
The n diagonal elements of the diagonal matrix D.
.TP 9
LD       (input) DOUBLE PRECISION array, dimension (N-1)
The n-1 elements L(i)*D(i).
.TP 9
LLD      (input) DOUBLE PRECISION array, dimension (N-1)
The n-1 elements L(i)*L(i)*D(i).
.TP 9
PIVMIN   (input) DOUBLE PRECISION
The minimum pivot in the Sturm sequence.
.TP 9
GAPTOL   (input) DOUBLE PRECISION
Tolerance that indicates when eigenvector entries are negligible
w.r.t. their contribution to the residual.
.TP 9
Z        (input/output) COMPLEX*16       array, dimension (N)
On input, all entries of Z must be set to 0.
On output, Z contains the (scaled) r-th column of the
inverse. The scaling is such that Z(R) equals 1.
.TP 9
WANTNC   (input) LOGICAL
Specifies whether NEGCNT has to be computed.
.TP 9
NEGCNT   (output) INTEGER
If WANTNC is .TRUE. then NEGCNT = the number of pivots < pivmin
in the  matrix factorization L D L^T, and NEGCNT = -1 otherwise.
.TP 9
ZTZ      (output) DOUBLE PRECISION
The square of the 2-norm of Z.
.TP 9
MINGMA   (output) DOUBLE PRECISION
The reciprocal of the largest (in magnitude) diagonal
element of the inverse of L D L^T - sigma I.
.TP 9
R        (input/output) INTEGER
The twist index for the twisted factorization used to
compute Z.
On input, 0 <= R <= N. If R is input as 0, R is set to
the index where (L D L^T - sigma I)^{-1} is largest
in magnitude. If 1 <= R <= N, R is unchanged.
On output, R contains the twist index used to compute Z.
Ideally, R designates the position of the maximum entry in the
eigenvector.
.TP 9
ISUPPZ   (output) INTEGER array, dimension (2)
The support of the vector in Z, i.e., the vector Z is
nonzero only in elements ISUPPZ(1) through ISUPPZ( 2 ).
.TP 9
NRMINV   (output) DOUBLE PRECISION
NRMINV = 1/SQRT( ZTZ )
.TP 9
RESID    (output) DOUBLE PRECISION
The residual of the FP vector.
RESID = ABS( MINGMA )/SQRT( ZTZ )
.TP 9
RQCORR   (output) DOUBLE PRECISION
The Rayleigh Quotient correction to LAMBDA.
RQCORR = MINGMA*TMP
.TP 9
WORK     (workspace) DOUBLE PRECISION array, dimension (4*N)
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

