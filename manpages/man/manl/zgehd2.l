.TH  ZGEHD2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZGEHD2 - a complex general matrix A to upper Hessenberg form H by a unitary similarity transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGEHD2(
N, ILO, IHI, A, LDA, TAU, WORK, INFO )
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
ZGEHD2 reduces a complex general matrix A to upper Hessenberg form H
by a unitary similarity transformation:  Q\(aq * A * Q = H .

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
It is assumed that A is already upper triangular in rows
and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
set by a previous call to ZGEBAL; otherwise they should be
set to 1 and N respectively. See Further Details.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the n by n general matrix to be reduced.
On exit, the upper triangle and the first subdiagonal of A
are overwritten with the upper Hessenberg matrix H, and the
elements below the first subdiagonal, with the array TAU,
represent the unitary matrix Q as a product of elementary
reflectors. See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
TAU     (output) COMPLEX*16 array, dimension (N-1)
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
The matrix Q is represented as a product of (ihi-ilo) elementary
reflectors
.br

   Q = H(ilo) H(ilo+1) . . . H(ihi-1).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v\(aq
.br

where tau is a complex scalar, and v is a complex vector with
v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on
exit in A(i+2:ihi,i), and tau in TAU(i).
.br

The contents of A are illustrated by the following example, with
n = 7, ilo = 2 and ihi = 6:
.br

on entry,                        on exit,
.br

( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a )
(     a   a   a   a   a   a )    (      a   h   h   h   h   a )
(     a   a   a   a   a   a )    (      h   h   h   h   h   h )
(     a   a   a   a   a   a )    (      v2  h   h   h   h   h )
(     a   a   a   a   a   a )    (      v2  v3  h   h   h   h )
(     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h )
(                         a )    (                          a )

where a denotes an element of the original matrix A, h denotes a
modified element of the upper Hessenberg matrix H, and vi denotes an
element of the vector defining H(i).
.br

