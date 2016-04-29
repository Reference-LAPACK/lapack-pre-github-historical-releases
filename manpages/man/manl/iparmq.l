.TH  IPARMQ 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
IPARMQ - program sets problem and machine dependent parameters  useful for xHSEQR and its subroutines
.SH SYNOPSIS
.TP 17
INTEGER FUNCTION
IPARMQ( ISPEC, NAME, OPTS, N, ILO, IHI, LWORK )
.TP 17
.ti +4
INTEGER
IHI, ILO, ISPEC, LWORK, N
.TP 17
.ti +4
CHARACTER
NAME*( * ), OPTS*( * )
.SH PURPOSE
     This program sets problem and machine dependent parameters
     useful for xHSEQR and its subroutines. It is called whenever 
     ILAENV is called with 12 <= ISPEC <= 16
.br

.SH ARGUMENTS
.TP 7
ISPEC  (input) integer scalar
ISPEC specifies which tunable parameter IPARMQ should
return.

ISPEC=12: (INMIN)  Matrices of order nmin or less
are sent directly to xLAHQR, the implicit
double shift QR algorithm.  NMIN must be
at least 11.

ISPEC=13: (INWIN)  Size of the deflation window.
This is best set greater than or equal to
the number of simultaneous shifts NS.
Larger matrices benefit from larger deflation
windows.

ISPEC=14: (INIBL) Determines when to stop nibbling and
invest in an (expensive) multi-shift QR sweep.
If the aggressive early deflation subroutine
finds LD converged eigenvalues from an order
NW deflation window and LD.GT.(NW*NIBBLE)/100,
then the next QR sweep is skipped and early
deflation is applied immediately to the
remaining active diagonal block.  Setting
IPARMQ(ISPEC=14) = 0 causes TTQRE to skip a
multi-shift QR sweep whenever early deflation
finds a converged eigenvalue.  Setting
IPARMQ(ISPEC=14) greater than or equal to 100
prevents TTQRE from skipping a multi-shift
QR sweep.

ISPEC=15: (NSHFTS) The number of simultaneous shifts in
a multi-shift QR iteration.

ISPEC=16: (IACC22) IPARMQ is set to 0, 1 or 2 with the
following meanings.
0:  During the multi-shift QR sweep,
xLAQR5 does not accumulate reflections and
does not use matrix-matrix multiply to
update the far-from-diagonal matrix
entries.
1:  During the multi-shift QR sweep,
xLAQR5 and/or xLAQRaccumulates reflections and uses
matrix-matrix multiply to update the
far-from-diagonal matrix entries.
2:  During the multi-shift QR sweep.
xLAQR5 accumulates reflections and takes
advantage of 2-by-2 block structure during
matrix-matrix multiplies.
(If xTRMM is slower than xGEMM, then
IPARMQ(ISPEC=16)=1 may be more efficient than
IPARMQ(ISPEC=16)=2 despite the greater level of
arithmetic work implied by the latter choice.)
.TP 8
NAME    (input) character string
Name of the calling subroutine
.TP 8
OPTS    (input) character string
This is a concatenation of the string arguments to
TTQRE.
.TP 8
N       (input) integer scalar
N is the order of the Hessenberg matrix H.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
It is assumed that H is already upper triangular
in rows and columns 1:ILO-1 and IHI+1:N.
.TP 8
LWORK   (input) integer scalar
The amount of workspace available.
.SH FURTHER DETAILS
     Little is known about how best to choose these parameters.
     It is possible to use different values of the parameters
     for each of CHSEQR, DHSEQR, SHSEQR and ZHSEQR.
.br

     It is probably best to choose different parameters for
     different matrices and different parameters at different
     times during the iteration, but this has not been
.br
     implemented --- yet.
.br


     The best choices of most of the parameters depend
.br
     in an ill-understood way on the relative execution
.br
     rate of xLAQR3 and xLAQR5 and on the nature of each
.br
     particular eigenvalue problem.  Experiment may be the
     only practical way to determine which choices are most
     effective.
.br

     Following is a list of default values supplied by IPARMQ.
     These defaults may be adjusted in order to attain better
     performance in any particular computational environment.

     IPARMQ(ISPEC=12) The xLAHQR vs xLAQR0 crossover point.
                      Default: 75. (Must be at least 11.)

     IPARMQ(ISPEC=13) Recommended deflation window size.
.br
                      This depends on ILO, IHI and NS, the
                      number of simultaneous shifts returned
                      by IPARMQ(ISPEC=15).  The default for
                      (IHI-ILO+1).LE.500 is NS.  The default
                      for (IHI-ILO+1).GT.500 is 3*NS/2.
.br

     IPARMQ(ISPEC=14) Nibble crossover point.  Default: 14.

     IPARMQ(ISPEC=15) Number of simultaneous shifts, NS.
.br
                      a multi-shift QR iteration.
.br

                      If IHI-ILO+1 is ...
.br

                      greater than      ...but less    ... the
                      or equal to ...      than        default is

                              0               30       NS =   2+
                             30               60       NS =   4+
                             60              150       NS =  10
                            150              590       NS =  **
                            590             3000       NS =  64
                           3000             6000       NS = 128
                           6000             infinity   NS = 256

                  (+)  By default matrices of this order are
                       passed to the implicit double shift routine
                       xLAHQR.  See IPARMQ(ISPEC=12) above.   These
                       values of NS are used only in case of a rare
                       xLAHQR failure.
.br

                  (**) The asterisks (**) indicate an ad-hoc
                       function increasing from 10 to 64.

     IPARMQ(ISPEC=16) Select structured matrix multiply.
.br
                      (See ISPEC=16 above for details.)
.br
                      Default: 3.
.br

