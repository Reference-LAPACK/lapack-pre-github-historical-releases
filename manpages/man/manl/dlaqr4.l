.TH  DLAQR4 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLAQR4 - compute the eigenvalues of a Hessenberg matrix H  and, optionally, the matrices T and Z from the Schur decomposition  H = Z T Z**T, where T is an upper quasi-triangular matrix (the  Schur form), and Z is the orthogonal matrix of Schur vectors
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAQR4(
WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
ILOZ, IHIZ, Z, LDZ, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, LWORK, N
.TP 19
.ti +4
LOGICAL
WANTT, WANTZ
.TP 19
.ti +4
DOUBLE
PRECISION H( LDH, * ), WI( * ), WORK( * ), WR( * ),
Z( LDZ, * )
.SH PURPOSE
   DLAQR4 computes the eigenvalues of a Hessenberg matrix H
   and, optionally, the matrices T and Z from the Schur decomposition
   H = Z T Z**T, where T is an upper quasi-triangular matrix (the
   Schur form), and Z is the orthogonal matrix of Schur vectors.

   Optionally Z may be postmultiplied into an input orthogonal
   matrix Q so that this routine can give the Schur factorization
   of a matrix A which has been reduced to the Hessenberg form H
   by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.

.SH ARGUMENTS
.TP 8
WANTT   (input) LOGICAL
= .TRUE. : the full Schur form T is required;
.br
= .FALSE.: only eigenvalues are required.
.TP 8
WANTZ   (input) LOGICAL
.br
= .TRUE. : the matrix of Schur vectors Z is required;
.br
= .FALSE.: Schur vectors are not required.
.TP 6
N     (input) INTEGER
The order of the matrix H.  N .GE. 0.
.TP 6
ILO   (input) INTEGER
IHI   (input) INTEGER
It is assumed that H is already upper triangular in rows
and columns 1:ILO-1 and IHI+1:N and, if ILO.GT.1,
H(ILO,ILO-1) is zero. ILO and IHI are normally set by a
previous call to DGEBAL, and then passed to DGEHRD when the
matrix output by DGEBAL is reduced to Hessenberg form.
Otherwise, ILO and IHI should be set to 1 and N,
respectively.  If N.GT.0, then 1.LE.ILO.LE.IHI.LE.N.
If N = 0, then ILO = 1 and IHI = 0.
.TP 6
H     (input/output) DOUBLE PRECISION array, dimension (LDH,N)
On entry, the upper Hessenberg matrix H.
On exit, if INFO = 0 and WANTT is .TRUE., then H contains
the upper quasi-triangular matrix T from the Schur
decomposition (the Schur form); 2-by-2 diagonal blocks
(corresponding to complex conjugate pairs of eigenvalues)
are returned in standard form, with H(i,i) = H(i+1,i+1)
and H(i+1,i)*H(i,i+1).LT.0. If INFO = 0 and WANTT is
.FALSE., then the contents of H are unspecified on exit.
(The output value of H when INFO.GT.0 is given under the
description of INFO below.)

This subroutine may explicitly set H(i,j) = 0 for i.GT.j and
j = 1, 2, ... ILO-1 or j = IHI+1, IHI+2, ... N.
.TP 6
LDH   (input) INTEGER
The leading dimension of the array H. LDH .GE. max(1,N).
.TP 6
WR    (output) DOUBLE PRECISION array, dimension (IHI)
WI    (output) DOUBLE PRECISION array, dimension (IHI)
The real and imaginary parts, respectively, of the computed
eigenvalues of H(ILO:IHI,ILO:IHI) are stored WR(ILO:IHI)
.br
and WI(ILO:IHI). If two eigenvalues are computed as a
complex conjugate pair, they are stored in consecutive
elements of WR and WI, say the i-th and (i+1)th, with
WI(i) .GT. 0 and WI(i+1) .LT. 0. If WANTT is .TRUE., then
the eigenvalues are stored in the same order as on the
diagonal of the Schur form returned in H, with
WR(i) = H(i,i) and, if H(i:i+1,i:i+1) is a 2-by-2 diagonal
block, WI(i) = sqrt(-H(i+1,i)*H(i,i+1)) and
WI(i+1) = -WI(i).
.TP 9
ILOZ     (input) INTEGER
IHIZ     (input) INTEGER
Specify the rows of Z to which transformations must be
applied if WANTZ is .TRUE..
1 .LE. ILOZ .LE. ILO; IHI .LE. IHIZ .LE. N.
.TP 6
Z     (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
If WANTZ is .FALSE., then Z is not referenced.
If WANTZ is .TRUE., then Z(ILO:IHI,ILOZ:IHIZ) is
.br
replaced by Z(ILO:IHI,ILOZ:IHIZ)*U where U is the
.br
orthogonal Schur factor of H(ILO:IHI,ILO:IHI).
(The output value of Z when INFO.GT.0 is given under
the description of INFO below.)
.TP 6
LDZ   (input) INTEGER
The leading dimension of the array Z.  if WANTZ is .TRUE.
then LDZ.GE.MAX(1,IHIZ).  Otherwize, LDZ.GE.1.
.TP 6
WORK  (workspace/output) DOUBLE PRECISION array, dimension LWORK
On exit, if LWORK = -1, WORK(1) returns an estimate of
the optimal value for LWORK.

LWORK (input) INTEGER
The dimension of the array WORK.  LWORK .GE. max(1,N)
is sufficient, but LWORK typically as large as 6*N may
be required for optimal performance.  A workspace query
to determine the optimal workspace size is recommended.

If LWORK = -1, then DLAQR4 does a workspace query.
In this case, DLAQR4 checks the input parameters and
estimates the optimal workspace size for the given
values of N, ILO and IHI.  The estimate is returned
in WORK(1).  No error message related to LWORK is
issued by XERBLA.  Neither H nor Z are accessed.
.TP 6
INFO  (output) INTEGER
=  0:  successful exit
.br
.GT. 0:  if INFO = i, DLAQR4 failed to compute all of
.br
the eigenvalues.  Elements 1:ilo-1 and i+1:n of WR
and WI contain those eigenvalues which have been
successfully computed.  (Failures are rare.)

If INFO .GT. 0 and WANT is .FALSE., then on exit,
the remaining unconverged eigenvalues are the eigen-
values of the upper Hessenberg matrix rows and
columns ILO through INFO of the final, output
value of H.

If INFO .GT. 0 and WANTT is .TRUE., then on exit
.TP 5
(*)  (initial value of H)*U  = U*(final value of H)

where U is an orthogonal matrix.  The final
value of H is upper Hessenberg and quasi-triangular
in rows and columns INFO+1 through IHI.

If INFO .GT. 0 and WANTZ is .TRUE., then on exit

(final value of Z(ILO:IHI,ILOZ:IHIZ)
=  (initial value of Z(ILO:IHI,ILOZ:IHIZ)*U

where U is the orthogonal matrix in (*) (regard-
less of the value of WANTT.)

If INFO .GT. 0 and WANTZ is .FALSE., then Z is not
accessed.
