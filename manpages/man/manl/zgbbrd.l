.TH  ZGBBRD 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZGBBRD - a complex general m-by-n band matrix A to real upper bidiagonal form B by a unitary transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGBBRD(
VECT, M, N, NCC, KL, KU, AB, LDAB, D, E, Q,
LDQ, PT, LDPT, C, LDC, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
VECT
.TP 19
.ti +4
INTEGER
INFO, KL, KU, LDAB, LDC, LDPT, LDQ, M, N, NCC
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX*16
AB( LDAB, * ), C( LDC, * ), PT( LDPT, * ),
Q( LDQ, * ), WORK( * )
.SH PURPOSE
ZGBBRD reduces a complex general m-by-n band matrix A to real upper
bidiagonal form B by a unitary transformation: Q\(aq * A * P = B.

The routine computes B, and optionally forms Q or P\(aq, or computes
Q\(aq*C for a given matrix C.
.br

.SH ARGUMENTS
.TP 8
VECT    (input) CHARACTER*1
Specifies whether or not the matrices Q and P\(aq are to be
formed.
= \(aqN\(aq: do not form Q or P\(aq;
.br
= \(aqQ\(aq: form Q only;
.br
= \(aqP\(aq: form P\(aq only;
.br
= \(aqB\(aq: form both.
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
NCC     (input) INTEGER
The number of columns of the matrix C.  NCC >= 0.
.TP 8
KL      (input) INTEGER
The number of subdiagonals of the matrix A. KL >= 0.
.TP 8
KU      (input) INTEGER
The number of superdiagonals of the matrix A. KU >= 0.
.TP 8
AB      (input/output) COMPLEX*16 array, dimension (LDAB,N)
On entry, the m-by-n band matrix A, stored in rows 1 to
KL+KU+1. The j-th column of A is stored in the j-th column of
the array AB as follows:
AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl).
On exit, A is overwritten by values generated during the
reduction.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array A. LDAB >= KL+KU+1.
.TP 8
D       (output) DOUBLE PRECISION array, dimension (min(M,N))
The diagonal elements of the bidiagonal matrix B.
.TP 8
E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
The superdiagonal elements of the bidiagonal matrix B.
.TP 8
Q       (output) COMPLEX*16 array, dimension (LDQ,M)
If VECT = \(aqQ\(aq or \(aqB\(aq, the m-by-m unitary matrix Q.
If VECT = \(aqN\(aq or \(aqP\(aq, the array Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.
LDQ >= max(1,M) if VECT = \(aqQ\(aq or \(aqB\(aq; LDQ >= 1 otherwise.
.TP 8
PT      (output) COMPLEX*16 array, dimension (LDPT,N)
If VECT = \(aqP\(aq or \(aqB\(aq, the n-by-n unitary matrix P\(aq.
If VECT = \(aqN\(aq or \(aqQ\(aq, the array PT is not referenced.
.TP 8
LDPT    (input) INTEGER
The leading dimension of the array PT.
LDPT >= max(1,N) if VECT = \(aqP\(aq or \(aqB\(aq; LDPT >= 1 otherwise.
.TP 8
C       (input/output) COMPLEX*16 array, dimension (LDC,NCC)
On entry, an m-by-ncc matrix C.
On exit, C is overwritten by Q\(aq*C.
C is not referenced if NCC = 0.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C.
LDC >= max(1,M) if NCC > 0; LDC >= 1 if NCC = 0.
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (max(M,N))
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (max(M,N))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
