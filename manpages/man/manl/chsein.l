.TH  CHSEIN 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CHSEIN - inverse iteration to find specified right and/or left eigenvectors of a complex upper Hessenberg matrix H
.SH SYNOPSIS
.TP 19
SUBROUTINE CHSEIN(
SIDE, EIGSRC, INITV, SELECT, N, H, LDH, W, VL,
LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL,
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
REAL
RWORK( * )
.TP 19
.ti +4
COMPLEX
H( LDH, * ), VL( LDVL, * ), VR( LDVR, * ),
W( * ), WORK( * )
.SH PURPOSE
CHSEIN uses inverse iteration to find specified right and/or left
eigenvectors of a complex upper Hessenberg matrix H.

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
Specifies the source of eigenvalues supplied in W:
.br
= \(aqQ\(aq: the eigenvalues were found using CHSEQR; thus, if
H has zero subdiagonal elements, and so is
block-triangular, then the j-th eigenvalue can be
assumed to be an eigenvalue of the block containing
the j-th row/column.  This property allows CHSEIN to
perform inverse iteration on just one diagonal block.
= \(aqN\(aq: no assumptions are made on the correspondence
between eigenvalues and diagonal blocks.  In this
case, CHSEIN must always perform inverse iteration
using the whole matrix H.
.TP 8
INITV   (input) CHARACTER*1
= \(aqN\(aq: no initial vectors are supplied;
.br
= \(aqU\(aq: user-supplied initial vectors are stored in the arrays
VL and/or VR.
.TP 8
SELECT  (input) LOGICAL array, dimension (N)
Specifies the eigenvectors to be computed. To select the
eigenvector corresponding to the eigenvalue W(j),
SELECT(j) must be set to .TRUE..
.TP 8
N       (input) INTEGER
The order of the matrix H.  N >= 0.
.TP 8
H       (input) COMPLEX array, dimension (LDH,N)
The upper Hessenberg matrix H.
.TP 8
LDH     (input) INTEGER
The leading dimension of the array H.  LDH >= max(1,N).
.TP 8
W       (input/output) COMPLEX array, dimension (N)
On entry, the eigenvalues of H.
On exit, the real parts of W may have been altered since
close eigenvalues are perturbed slightly in searching for
independent eigenvectors.
.TP 8
VL      (input/output) COMPLEX array, dimension (LDVL,MM)
On entry, if INITV = \(aqU\(aq and SIDE = \(aqL\(aq or \(aqB\(aq, VL must
contain starting vectors for the inverse iteration for the
left eigenvectors; the starting vector for each eigenvector
must be in the same column in which the eigenvector will be
stored.
On exit, if SIDE = \(aqL\(aq or \(aqB\(aq, the left eigenvectors
specified by SELECT will be stored consecutively in the
columns of VL, in the same order as their eigenvalues.
If SIDE = \(aqR\(aq, VL is not referenced.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL.
LDVL >= max(1,N) if SIDE = \(aqL\(aq or \(aqB\(aq; LDVL >= 1 otherwise.
.TP 8
VR      (input/output) COMPLEX array, dimension (LDVR,MM)
On entry, if INITV = \(aqU\(aq and SIDE = \(aqR\(aq or \(aqB\(aq, VR must
contain starting vectors for the inverse iteration for the
right eigenvectors; the starting vector for each eigenvector
must be in the same column in which the eigenvector will be
stored.
On exit, if SIDE = \(aqR\(aq or \(aqB\(aq, the right eigenvectors
specified by SELECT will be stored consecutively in the
columns of VR, in the same order as their eigenvalues.
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
store the eigenvectors (= the number of .TRUE. elements in
SELECT).
.TP 8
WORK    (workspace) COMPLEX array, dimension (N*N)
.TP 8
RWORK   (workspace) REAL array, dimension (N)
.TP 8
IFAILL  (output) INTEGER array, dimension (MM)
If SIDE = \(aqL\(aq or \(aqB\(aq, IFAILL(i) = j > 0 if the left
eigenvector in the i-th column of VL (corresponding to the
eigenvalue w(j)) failed to converge; IFAILL(i) = 0 if the
eigenvector converged satisfactorily.
If SIDE = \(aqR\(aq, IFAILL is not referenced.
.TP 8
IFAILR  (output) INTEGER array, dimension (MM)
If SIDE = \(aqR\(aq or \(aqB\(aq, IFAILR(i) = j > 0 if the right
eigenvector in the i-th column of VR (corresponding to the
eigenvalue w(j)) failed to converge; IFAILR(i) = 0 if the
eigenvector converged satisfactorily.
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

