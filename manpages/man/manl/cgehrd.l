.TH  CGEHRD 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CGEHRD - a complex general matrix A to upper Hessenberg form H by an unitary similarity transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE CGEHRD(
N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LWORK, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
CGEHRD reduces a complex general matrix A to upper Hessenberg form H by
an unitary similarity transformation:  Q\(aq * A * Q = H .

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
It is assumed that A is already upper triangular in rows
and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
set by a previous call to CGEBAL; otherwise they should be
set to 1 and N respectively. See Further Details.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the N-by-N general matrix to be reduced.
On exit, the upper triangle and the first subdiagonal of A
are overwritten with the upper Hessenberg matrix H, and the
elements below the first subdiagonal, with the array TAU,
represent the unitary matrix Q as a product of elementary
reflectors. See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
TAU     (output) COMPLEX array, dimension (N-1)
The scalar factors of the elementary reflectors (see Further
Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to
zero.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of the array WORK.  LWORK >= max(1,N).
For optimum performance LWORK >= N*NB, where NB is the
optimal blocksize.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
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

This file is a slight modification of LAPACK-3.0\(aqs CGEHRD
subroutine incorporating improvements proposed by Quintana-Orti and
Van de Geijn (2005). 
.br

