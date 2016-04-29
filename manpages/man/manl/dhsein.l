.TH  DHSEIN 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DHSEIN - inverse iteration to find specified right and/or left eigenvectors of a real upper Hessenberg matrix H
.SH SYNOPSIS
.TP 19
SUBROUTINE DHSEIN(
SIDE, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,
VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL,
IFAILR, INFO )
.TP 19
.ti +4
CHARACTER
EIGSRC, INITV, SIDE
.TP 19
.ti +4
INTEGER
INFO, LDH, LDVL, LDVR, M, MM, N
.TP 19
.ti +4
LOGICAL
SELECT( * )
.TP 19
.ti +4
INTEGER
IFAILL( * ), IFAILR( * )
.TP 19
.ti +4
DOUBLE
PRECISION H( LDH, * ), VL( LDVL, * ), VR( LDVR, * ),
WI( * ), WORK( * ), WR( * )
.SH PURPOSE
DHSEIN uses inverse iteration to find specified right and/or left
eigenvectors of a real upper Hessenberg matrix H.

The right eigenvector x and the left eigenvector y of the matrix H
corresponding to an eigenvalue w are defined by:
.br

             H * x = w * x,     y**h * H = w * y**h
.br

where y**h denotes the conjugate transpose of the vector y.

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= \(aqR\(aq: compute right eigenvectors only;
.br
= \(aqL\(aq: compute left eigenvectors only;
.br
= \(aqB\(aq: compute both right and left eigenvectors.
.TP 8
EIGSRC  (input) CHARACTER*1
.br
Specifies the source of eigenvalues supplied in (WR,WI):
.br
= \(aqQ\(aq: the eigenvalues were found using DHSEQR; thus, if
H has zero subdiagonal elements, and so is
block-triangular, then the j-th eigenvalue can be
assumed to be an eigenvalue of the block containing
the j-th row/column.  This property allows DHSEIN to
perform inverse iteration on just one diagonal block.
= \(aqN\(aq: no assumptions are made on the correspondence
between eigenvalues and diagonal blocks.  In this
case, DHSEIN must always perform inverse iteration
using the whole matrix H.
.TP 8
INITV   (input) CHARACTER*1
= \(aqN\(aq: no initial vectors are supplied;
.br
= \(aqU\(aq: user-supplied initial vectors are stored in the arrays
VL and/or VR.
.TP 8
SELECT  (input/output) LOGICAL array, dimension (N)
Specifies the eigenvectors to be computed. To select the
real eigenvector corresponding to a real eigenvalue WR(j),
SELECT(j) must be set to .TRUE.. To select the complex
eigenvector corresponding to a complex eigenvalue
(WR(j),WI(j)), with complex conjugate (WR(j+1),WI(j+1)),
either SELECT(j) or SELECT(j+1) or both must be set to
.TRUE.; then on exit SELECT(j) is .TRUE. and SELECT(j+1) is
.FALSE..
.TP 8
N       (input) INTEGER
The order of the matrix H.  N >= 0.
.TP 8
H       (input) DOUBLE PRECISION array, dimension (LDH,N)
The upper Hessenberg matrix H.
.TP 8
LDH     (input) INTEGER
The leading dimension of the array H.  LDH >= max(1,N).
.TP 8
WR      (input/output) DOUBLE PRECISION array, dimension (N)
WI      (input) DOUBLE PRECISION array, dimension (N)
On entry, the real and imaginary parts of the eigenvalues of
H; a complex conjugate pair of eigenvalues must be stored in
consecutive elements of WR and WI.
On exit, WR may have been altered since close eigenvalues
are perturbed slightly in searching for independent
eigenvectors.
.TP 8
VL      (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
On entry, if INITV = \(aqU\(aq and SIDE = \(aqL\(aq or \(aqB\(aq, VL must
contain starting vectors for the inverse iteration for the
left eigenvectors; the starting vector for each eigenvector
must be in the same column(s) in which the eigenvector will
be stored.
On exit, if SIDE = \(aqL\(aq or \(aqB\(aq, the left eigenvectors
specified by SELECT will be stored consecutively in the
columns of VL, in the same order as their eigenvalues. A
complex eigenvector corresponding to a complex eigenvalue is
stored in two consecutive columns, the first holding the real
part and the second the imaginary part.
If SIDE = \(aqR\(aq, VL is not referenced.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL.
LDVL >= max(1,N) if SIDE = \(aqL\(aq or \(aqB\(aq; LDVL >= 1 otherwise.
.TP 8
VR      (input/output) DOUBLE PRECISION array, dimension (LDVR,MM)
On entry, if INITV = \(aqU\(aq and SIDE = \(aqR\(aq or \(aqB\(aq, VR must
contain starting vectors for the inverse iteration for the
right eigenvectors; the starting vector for each eigenvector
must be in the same column(s) in which the eigenvector will
be stored.
On exit, if SIDE = \(aqR\(aq or \(aqB\(aq, the right eigenvectors
specified by SELECT will be stored consecutively in the
columns of VR, in the same order as their eigenvalues. A
complex eigenvector corresponding to a complex eigenvalue is
stored in two consecutive columns, the first holding the real
part and the second the imaginary part.
If SIDE = \(aqL\(aq, VR is not referenced.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.
LDVR >= max(1,N) if SIDE = \(aqR\(aq or \(aqB\(aq; LDVR >= 1 otherwise.
.TP 8
MM      (input) INTEGER
The number of columns in the arrays VL and/or VR. MM >= M.
.TP 8
M       (output) INTEGER
The number of columns in the arrays VL and/or VR required to
store the eigenvectors; each selected real eigenvector
occupies one column and each selected complex eigenvector
occupies two columns.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension ((N+2)*N)
.TP 8
IFAILL  (output) INTEGER array, dimension (MM)
If SIDE = \(aqL\(aq or \(aqB\(aq, IFAILL(i) = j > 0 if the left
eigenvector in the i-th column of VL (corresponding to the
eigenvalue w(j)) failed to converge; IFAILL(i) = 0 if the
eigenvector converged satisfactorily. If the i-th and (i+1)th
columns of VL hold a complex eigenvector, then IFAILL(i) and
IFAILL(i+1) are set to the same value.
If SIDE = \(aqR\(aq, IFAILL is not referenced.
.TP 8
IFAILR  (output) INTEGER array, dimension (MM)
If SIDE = \(aqR\(aq or \(aqB\(aq, IFAILR(i) = j > 0 if the right
eigenvector in the i-th column of VR (corresponding to the
eigenvalue w(j)) failed to converge; IFAILR(i) = 0 if the
eigenvector converged satisfactorily. If the i-th and (i+1)th
columns of VR hold a complex eigenvector, then IFAILR(i) and
IFAILR(i+1) are set to the same value.
If SIDE = \(aqL\(aq, IFAILR is not referenced.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, i is the number of eigenvectors which
failed to converge; see IFAILL and IFAILR for further
details.
.SH FURTHER DETAILS
Each eigenvector is normalized so that the element of largest
magnitude has magnitude 1; here the magnitude of a complex number
(x,y) is taken to be |x|+|y|.
.br

