.TH  DBDSQR 1 "February 2007" " LAPACK routine (version 3.1.1) " " LAPACK routine (version 3.1.1) " 
.SH NAME
DBDSQR - the singular values and, optionally, the right and/or left singular vectors from the singular value decomposition (SVD) of a real N-by-N (upper or lower) bidiagonal matrix B using the implicit zero-shift QR algorithm
.SH SYNOPSIS
.TP 19
SUBROUTINE DBDSQR(
UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,
LDU, C, LDC, WORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU
.TP 19
.ti +4
DOUBLE
PRECISION C( LDC, * ), D( * ), E( * ), U( LDU, * ),
VT( LDVT, * ), WORK( * )
.SH PURPOSE
DBDSQR computes the singular values and, optionally, the right and/or
left singular vectors from the singular value decomposition (SVD) of
a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
zero-shift QR algorithm.  The SVD of B has the form

   B = Q * S * P**T
.br

where S is the diagonal matrix of singular values, Q is an orthogonal
matrix of left singular vectors, and P is an orthogonal matrix of
right singular vectors.  If left singular vectors are requested, this
subroutine actually returns U*Q instead of Q, and, if right singular
vectors are requested, this subroutine returns P**T*VT instead of
P**T, for given real input matrices U and VT.  When U and VT are the
orthogonal matrices that reduce a general matrix A to bidiagonal
form:  A = U*B*VT, as computed by DGEBRD, then
.br

   A = (U*Q) * S * (P**T*VT)
.br

is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
for a given real input matrix C.
.br

See "Computing  Small Singular Values of Bidiagonal Matrices With
Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
no. 5, pp. 873-912, Sept 1990) and
.br
"Accurate singular values and differential qd algorithms," by
B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
Department, University of California at Berkeley, July 1992
for a detailed description of the algorithm.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  B is upper bidiagonal;
.br
= \(aqL\(aq:  B is lower bidiagonal.
.TP 8
N       (input) INTEGER
The order of the matrix B.  N >= 0.
.TP 8
NCVT    (input) INTEGER
The number of columns of the matrix VT. NCVT >= 0.
.TP 8
NRU     (input) INTEGER
The number of rows of the matrix U. NRU >= 0.
.TP 8
NCC     (input) INTEGER
The number of columns of the matrix C. NCC >= 0.
.TP 8
D       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the n diagonal elements of the bidiagonal matrix B.
On exit, if INFO=0, the singular values of B in decreasing
order.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N-1)
On entry, the N-1 offdiagonal elements of the bidiagonal
matrix B. 
On exit, if INFO = 0, E is destroyed; if INFO > 0, D and E
will contain the diagonal and superdiagonal elements of a
bidiagonal matrix orthogonally equivalent to the one given
as input.
.TP 8
VT      (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
On entry, an N-by-NCVT matrix VT.
On exit, VT is overwritten by P**T * VT.
Not referenced if NCVT = 0.
.TP 8
LDVT    (input) INTEGER
The leading dimension of the array VT.
LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
.TP 8
U       (input/output) DOUBLE PRECISION array, dimension (LDU, N)
On entry, an NRU-by-N matrix U.
On exit, U is overwritten by U * Q.
Not referenced if NRU = 0.
.TP 8
LDU     (input) INTEGER
The leading dimension of the array U.  LDU >= max(1,NRU).
.TP 8
C       (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
On entry, an N-by-NCC matrix C.
On exit, C is overwritten by Q**T * C.
Not referenced if NCC = 0.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C.
LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
if NCVT = NRU = NCC = 0, (max(1, 4*N)) otherwise
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  If INFO = -i, the i-th argument had an illegal value
.br
> 0:  the algorithm did not converge; D and E contain the
elements of a bidiagonal matrix which is orthogonally
similar to the input matrix B;  if INFO = i, i
elements of E have not converged to zero.
.SH PARAMETERS
.TP 8
TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
TOLMUL controls the convergence criterion of the QR loop.
If it is positive, TOLMUL*EPS is the desired relative
precision in the computed singular values.
If it is negative, abs(TOLMUL*EPS*sigma_max) is the
desired absolute accuracy in the computed singular
values (corresponds to relative accuracy
abs(TOLMUL*EPS) in the largest singular value.
abs(TOLMUL) should be between 1 and 1/EPS, and preferably
between 10 (for fast convergence) and .1/EPS
(for there to be some accuracy in the results).
Default is to lose at either one eighth or 2 of the
available decimal digits in each computed singular value
(whichever is smaller).
.TP 8
MAXITR  INTEGER, default = 6
MAXITR controls the maximum number of passes of the
algorithm through its inner loop. The algorithms stops
(and so fails to converge) if the number of passes
through the inner loop exceeds MAXITR*N**2.
