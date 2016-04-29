.TH  CLAPMT 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
CLAPMT - the columns of the M by N matrix X as specified by the permutation K(1),K(2),...,K(N) of the integers 1,...,N
.SH SYNOPSIS
.TP 19
SUBROUTINE CLAPMT(
FORWRD, M, N, X, LDX, K )
.TP 19
.ti +4
LOGICAL
FORWRD
.TP 19
.ti +4
INTEGER
LDX, M, N
.TP 19
.ti +4
INTEGER
K( * )
.TP 19
.ti +4
COMPLEX
X( LDX, * )
.SH PURPOSE
CLAPMT rearranges the columns of the M by N matrix X as specified
by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
If FORWRD = .TRUE.,  forward permutation:
.br

     X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
.br

If FORWRD = .FALSE., backward permutation:
.br

     X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
.br

.SH ARGUMENTS
.TP 8
FORWRD  (input) LOGICAL
= .TRUE., forward permutation
= .FALSE., backward permutation
.TP 8
M       (input) INTEGER
The number of rows of the matrix X. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix X. N >= 0.
.TP 8
X       (input/output) COMPLEX array, dimension (LDX,N)
On entry, the M by N matrix X.
On exit, X contains the permuted matrix X.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X, LDX >= MAX(1,M).
.TP 8
K       (input/output) INTEGER array, dimension (N)
On entry, K contains the permutation vector. K is used as
internal workspace, but reset to its original value on
output.
