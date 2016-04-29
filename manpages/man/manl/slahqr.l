.TH  SLAHQR 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAHQR - i an auxiliary routine called by SHSEQR to update the  eigenvalues and Schur decomposition already computed by SHSEQR, by  dealing with the Hessenberg submatrix in rows and columns ILO to  IHI
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAHQR(
WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
ILOZ, IHIZ, Z, LDZ, INFO )
.TP 19
.ti +4
INTEGER
IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, N
.TP 19
.ti +4
LOGICAL
WANTT, WANTZ
.TP 19
.ti +4
REAL
H( LDH, * ), WI( * ), WR( * ), Z( LDZ, * )
.SH PURPOSE
   SLAHQR is an auxiliary routine called by SHSEQR to update the
   eigenvalues and Schur decomposition already computed by SHSEQR, by
   dealing with the Hessenberg submatrix in rows and columns ILO to
   IHI.

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
.TP 8
N       (input) INTEGER
The order of the matrix H.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
It is assumed that H is already upper quasi-triangular in
rows and columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless
ILO = 1). SLAHQR works primarily with the Hessenberg
submatrix in rows and columns ILO to IHI, but applies
transformations to all of H if WANTT is .TRUE..
1 <= ILO <= max(1,IHI); IHI <= N.
.TP 8
H       (input/output) REAL array, dimension (LDH,N)
On entry, the upper Hessenberg matrix H.
On exit, if INFO is zero and if WANTT is .TRUE., H is upper
quasi-triangular in rows and columns ILO:IHI, with any
2-by-2 diagonal blocks in standard form. If INFO is zero
and WANTT is .FALSE., the contents of H are unspecified on
exit.  The output state of H if INFO is nonzero is given
below under the description of INFO.
.TP 8
LDH     (input) INTEGER
The leading dimension of the array H. LDH >= max(1,N).
.TP 8
WR      (output) REAL array, dimension (N)
WI      (output) REAL array, dimension (N)
The real and imaginary parts, respectively, of the computed
eigenvalues ILO to IHI are stored in the corresponding
elements of WR and WI. If two eigenvalues are computed as a
complex conjugate pair, they are stored in consecutive
elements of WR and WI, say the i-th and (i+1)th, with
WI(i) > 0 and WI(i+1) < 0. If WANTT is .TRUE., the
eigenvalues are stored in the same order as on the diagonal
of the Schur form returned in H, with WR(i) = H(i,i), and, if
H(i:i+1,i:i+1) is a 2-by-2 diagonal block,
WI(i) = sqrt(H(i+1,i)*H(i,i+1)) and WI(i+1) = -WI(i).
.TP 8
ILOZ    (input) INTEGER
IHIZ    (input) INTEGER
Specify the rows of Z to which transformations must be
applied if WANTZ is .TRUE..
1 <= ILOZ <= ILO; IHI <= IHIZ <= N.
.TP 8
Z       (input/output) REAL array, dimension (LDZ,N)
If WANTZ is .TRUE., on entry Z must contain the current
matrix Z of transformations accumulated by SHSEQR, and on
exit Z has been updated; transformations are applied only to
the submatrix Z(ILOZ:IHIZ,ILO:IHI).
If WANTZ is .FALSE., Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z. LDZ >= max(1,N).
.TP 8
INFO    (output) INTEGER
=   0: successful exit
.br
.GT. 0: If INFO = i, SLAHQR failed to compute all the
eigenvalues ILO to IHI in a total of 30 iterations
per eigenvalue; elements i+1:ihi of WR and WI
contain those eigenvalues which have been
successfully computed.

If INFO .GT. 0 and WANTT is .FALSE., then on exit,
the remaining unconverged eigenvalues are the
eigenvalues of the upper Hessenberg matrix rows
and columns ILO thorugh INFO of the final, output
value of H.

If INFO .GT. 0 and WANTT is .TRUE., then on exit
(*)       (initial value of H)*U  = U*(final value of H)
where U is an orthognal matrix.    The final
value of H is upper Hessenberg and triangular in
rows and columns INFO+1 through IHI.

If INFO .GT. 0 and WANTZ is .TRUE., then on exit
(final value of Z)  = (initial value of Z)*U
where U is the orthogonal matrix in (*)
(regardless of the value of WANTT.)
.SH FURTHER DETAILS
   02-96 Based on modifications by
.br
   David Day, Sandia National Laboratory, USA
.br

   12-04 Further modifications by
.br
   Ralph Byers, University of Kansas, USA
.br

     This is a modified version of SLAHQR from LAPACK version 3.0.
     It is (1) more robust against overflow and underflow and
     (2) adopts the more conservative Ahues & Tisseur stopping
     criterion (LAWN 122, 1997).
.br

