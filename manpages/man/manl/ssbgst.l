.TH  SSBGST 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SSBGST - a real symmetric-definite banded generalized eigenproblem A*x = lambda*B*x to standard form C*y = lambda*y,
.SH SYNOPSIS
.TP 19
SUBROUTINE SSBGST(
VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,
LDX, WORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO, VECT
.TP 19
.ti +4
INTEGER
INFO, KA, KB, LDAB, LDBB, LDX, N
.TP 19
.ti +4
REAL
AB( LDAB, * ), BB( LDBB, * ), WORK( * ),
X( LDX, * )
.SH PURPOSE
SSBGST reduces a real symmetric-definite banded generalized
eigenproblem  A*x = lambda*B*x  to standard form  C*y = lambda*y,
such that C has the same bandwidth as A.
.br

B must have been previously factorized as S**T*S by SPBSTF, using a
split Cholesky factorization. A is overwritten by C = X**T*A*X, where
X = S**(-1)*Q and Q is an orthogonal matrix chosen to preserve the
bandwidth of A.
.br

.SH ARGUMENTS
.TP 8
VECT    (input) CHARACTER*1
= \(aqN\(aq:  do not form the transformation matrix X;
.br
= \(aqV\(aq:  form X.
.TP 8
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
KA      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq.  KA >= 0.
.TP 8
KB      (input) INTEGER
The number of superdiagonals of the matrix B if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq.  KA >= KB >= 0.
.TP 8
AB      (input/output) REAL array, dimension (LDAB,N)
On entry, the upper or lower triangle of the symmetric band
matrix A, stored in the first ka+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = \(aqU\(aq, AB(ka+1+i-j,j) = A(i,j) for max(1,j-ka)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+ka).

On exit, the transformed matrix X**T*A*X, stored in the same
format as A.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KA+1.
.TP 8
BB      (input) REAL array, dimension (LDBB,N)
The banded factor S from the split Cholesky factorization of
B, as returned by SPBSTF, stored in the first KB+1 rows of
the array.
.TP 8
LDBB    (input) INTEGER
The leading dimension of the array BB.  LDBB >= KB+1.
.TP 8
X       (output) REAL array, dimension (LDX,N)
If VECT = \(aqV\(aq, the n-by-n matrix X.
If VECT = \(aqN\(aq, the array X is not referenced.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.
LDX >= max(1,N) if VECT = \(aqV\(aq; LDX >= 1 otherwise.
.TP 8
WORK    (workspace) REAL array, dimension (2*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
