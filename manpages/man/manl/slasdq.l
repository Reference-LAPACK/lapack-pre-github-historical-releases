.TH  SLASDQ 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLASDQ - the singular value decomposition (SVD) of a real (upper or lower) bidiagonal matrix with diagonal D and offdiagonal E, accumulating the transformations if desired
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASDQ(
UPLO, SQRE, N, NCVT, NRU, NCC, D, E, VT, LDVT,
U, LDU, C, LDC, WORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU, SQRE
.TP 19
.ti +4
REAL
C( LDC, * ), D( * ), E( * ), U( LDU, * ),
VT( LDVT, * ), WORK( * )
.SH PURPOSE
SLASDQ computes the singular value decomposition (SVD) of a real
(upper or lower) bidiagonal matrix with diagonal D and offdiagonal
E, accumulating the transformations if desired. Letting B denote
the input bidiagonal matrix, the algorithm computes orthogonal
matrices Q and P such that B = Q * S * P\(aq (P\(aq denotes the transpose
of P). The singular values S are overwritten on D.
.br

The input matrix U  is changed to U  * Q  if desired.
.br
The input matrix VT is changed to P\(aq * VT if desired.
.br
The input matrix C  is changed to Q\(aq * C  if desired.
.br

See "Computing  Small Singular Values of Bidiagonal Matrices With
Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
LAPACK Working Note #3, for a detailed description of the algorithm.

.SH ARGUMENTS
.TP 6
UPLO  (input) CHARACTER*1
On entry, UPLO specifies whether the input bidiagonal matrix
is upper or lower bidiagonal, and wether it is square are
not.
UPLO = \(aqU\(aq or \(aqu\(aq   B is upper bidiagonal.
UPLO = \(aqL\(aq or \(aql\(aq   B is lower bidiagonal.
.TP 6
SQRE  (input) INTEGER
= 0: then the input matrix is N-by-N.
.br
= 1: then the input matrix is N-by-(N+1) if UPLU = \(aqU\(aq and
(N+1)-by-N if UPLU = \(aqL\(aq.

The bidiagonal matrix has
N = NL + NR + 1 rows and
M = N + SQRE >= N columns.
.TP 6
N     (input) INTEGER
On entry, N specifies the number of rows and columns
in the matrix. N must be at least 0.
.TP 6
NCVT  (input) INTEGER
On entry, NCVT specifies the number of columns of
the matrix VT. NCVT must be at least 0.
.TP 6
NRU   (input) INTEGER
On entry, NRU specifies the number of rows of
the matrix U. NRU must be at least 0.
.TP 6
NCC   (input) INTEGER
On entry, NCC specifies the number of columns of
the matrix C. NCC must be at least 0.
.TP 6
D     (input/output) REAL array, dimension (N)
On entry, D contains the diagonal entries of the
bidiagonal matrix whose SVD is desired. On normal exit,
D contains the singular values in ascending order.
.TP 6
E     (input/output) REAL array.
dimension is (N-1) if SQRE = 0 and N if SQRE = 1.
On entry, the entries of E contain the offdiagonal entries
of the bidiagonal matrix whose SVD is desired. On normal
exit, E will contain 0. If the algorithm does not converge,
D and E will contain the diagonal and superdiagonal entries
of a bidiagonal matrix orthogonally equivalent to the one
given as input.
.TP 6
VT    (input/output) REAL array, dimension (LDVT, NCVT)
On entry, contains a matrix which on exit has been
premultiplied by P\(aq, dimension N-by-NCVT if SQRE = 0
and (N+1)-by-NCVT if SQRE = 1 (not referenced if NCVT=0).
.TP 6
LDVT  (input) INTEGER
On entry, LDVT specifies the leading dimension of VT as
declared in the calling (sub) program. LDVT must be at
least 1. If NCVT is nonzero LDVT must also be at least N.
.TP 6
U     (input/output) REAL array, dimension (LDU, N)
On entry, contains a  matrix which on exit has been
postmultiplied by Q, dimension NRU-by-N if SQRE = 0
and NRU-by-(N+1) if SQRE = 1 (not referenced if NRU=0).
.TP 6
LDU   (input) INTEGER
On entry, LDU  specifies the leading dimension of U as
declared in the calling (sub) program. LDU must be at
least max( 1, NRU ) .
.TP 6
C     (input/output) REAL array, dimension (LDC, NCC)
On entry, contains an N-by-NCC matrix which on exit
has been premultiplied by Q\(aq  dimension N-by-NCC if SQRE = 0
and (N+1)-by-NCC if SQRE = 1 (not referenced if NCC=0).
.TP 6
LDC   (input) INTEGER
On entry, LDC  specifies the leading dimension of C as
declared in the calling (sub) program. LDC must be at
least 1. If NCC is nonzero, LDC must also be at least N.
.TP 6
WORK  (workspace) REAL array, dimension (4*N)
Workspace. Only referenced if one of NCVT, NRU, or NCC is
nonzero, and if N is at least 2.
.TP 6
INFO  (output) INTEGER
On exit, a value of 0 indicates a successful exit.
If INFO < 0, argument number -INFO is illegal.
If INFO > 0, the algorithm did not converge, and INFO
specifies how many superdiagonals did not converge.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Huan Ren, Computer Science Division, University of
   California at Berkeley, USA
.br

