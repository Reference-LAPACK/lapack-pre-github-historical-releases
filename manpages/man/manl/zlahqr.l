.TH  ZLAHQR 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLAHQR - i an auxiliary routine called by CHSEQR to update the  eigenvalues and Schur decomposition already computed by CHSEQR, by  dealing with the Hessenberg submatrix in rows and columns ILO to  IHI
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAHQR(
WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILOZ,
IHIZ, Z, LDZ, INFO )
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
COMPLEX*16
H( LDH, * ), W( * ), Z( LDZ, * )
.SH PURPOSE
   ZLAHQR is an auxiliary routine called by CHSEQR to update the
   eigenvalues and Schur decomposition already computed by CHSEQR, by
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
It is assumed that H is already upper triangular in rows and
columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless ILO = 1).
ZLAHQR works primarily with the Hessenberg submatrix in rows
and columns ILO to IHI, but applies transformations to all of
H if WANTT is .TRUE..
1 <= ILO <= max(1,IHI); IHI <= N.
.TP 8
H       (input/output) COMPLEX*16 array, dimension (LDH,N)
On entry, the upper Hessenberg matrix H.
On exit, if INFO is zero and if WANTT is .TRUE., then H
is upper triangular in rows and columns ILO:IHI.  If INFO
is zero and if WANTT is .FALSE., then the contents of H
are unspecified on exit.  The output state of H in case
INF is positive is below under the description of INFO.
.TP 8
LDH     (input) INTEGER
The leading dimension of the array H. LDH >= max(1,N).
.TP 8
W       (output) COMPLEX*16 array, dimension (N)
The computed eigenvalues ILO to IHI are stored in the
corresponding elements of W. If WANTT is .TRUE., the
eigenvalues are stored in the same order as on the diagonal
of the Schur form returned in H, with W(i) = H(i,i).
.TP 8
ILOZ    (input) INTEGER
IHIZ    (input) INTEGER
Specify the rows of Z to which transformations must be
applied if WANTZ is .TRUE..
1 <= ILOZ <= ILO; IHI <= IHIZ <= N.
.TP 8
Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)
If WANTZ is .TRUE., on entry Z must contain the current
matrix Z of transformations accumulated by CHSEQR, and on
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
.GT. 0: if INFO = i, ZLAHQR failed to compute all the
eigenvalues ILO to IHI in a total of 30 iterations
per eigenvalue; elements i+1:ihi of W contain
those eigenvalues which have been successfully
computed.

If INFO .GT. 0 and WANTT is .FALSE., then on exit,
the remaining unconverged eigenvalues are the
eigenvalues of the upper Hessenberg matrix
rows and columns ILO thorugh INFO of the final,
output value of H.

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

     This is a modified version of ZLAHQR from LAPACK version 3.0.
     It is (1) more robust against overflow and underflow and
     (2) adopts the more conservative Ahues & Tisseur stopping
     criterion (LAWN 122, 1997).
.br

