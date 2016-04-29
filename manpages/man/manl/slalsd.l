.TH  SLALSD 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLALSD - the singular value decomposition of A to solve the least squares problem of finding X to minimize the Euclidean norm of each column of A*X-B, where A is N-by-N upper bidiagonal, and X and B are N-by-NRHS
.SH SYNOPSIS
.TP 19
SUBROUTINE SLALSD(
UPLO, SMLSIZ, N, NRHS, D, E, B, LDB, RCOND,
RANK, WORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDB, N, NRHS, RANK, SMLSIZ
.TP 19
.ti +4
REAL
RCOND
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
B( LDB, * ), D( * ), E( * ), WORK( * )
.SH PURPOSE
SLALSD uses the singular value decomposition of A to solve the least
squares problem of finding X to minimize the Euclidean norm of each
column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
are N-by-NRHS. The solution X overwrites B.

The singular values of A smaller than RCOND times the largest
singular value are treated as zero in solving the least squares
problem; in this case a minimum norm solution is returned.
The actual singular values are returned in D in ascending order.

This code makes very mild assumptions about floating point
arithmetic. It will work on machines with a guard digit in
add/subtract, or on those binary machines without guard digits
which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
.br

.SH ARGUMENTS
.TP 7
UPLO   (input) CHARACTER*1
= \(aqU\(aq: D and E define an upper bidiagonal matrix.
.br
= \(aqL\(aq: D and E define a  lower bidiagonal matrix.

SMLSIZ (input) INTEGER
The maximum size of the subproblems at the bottom of the
computation tree.
.TP 7
N      (input) INTEGER
The dimension of the  bidiagonal matrix.  N >= 0.
.TP 7
NRHS   (input) INTEGER
The number of columns of B. NRHS must be at least 1.
.TP 7
D      (input/output) REAL array, dimension (N)
On entry D contains the main diagonal of the bidiagonal
matrix. On exit, if INFO = 0, D contains its singular values.
.TP 7
E      (input/output) REAL array, dimension (N-1)
Contains the super-diagonal entries of the bidiagonal matrix.
On exit, E has been destroyed.
.TP 7
B      (input/output) REAL array, dimension (LDB,NRHS)
On input, B contains the right hand sides of the least
squares problem. On output, B contains the solution X.
.TP 7
LDB    (input) INTEGER
The leading dimension of B in the calling subprogram.
LDB must be at least max(1,N).
.TP 7
RCOND  (input) REAL
The singular values of A less than or equal to RCOND times
the largest singular value are treated as zero in solving
the least squares problem. If RCOND is negative,
machine precision is used instead.
For example, if diag(S)*X=B were the least squares problem,
where diag(S) is a diagonal matrix of singular values, the
solution would be X(i) = B(i) / S(i) if S(i) is greater than
RCOND*max(S), and X(i) = 0 if S(i) is less than or equal to
RCOND*max(S).
.TP 7
RANK   (output) INTEGER
The number of singular values of A greater than RCOND times
the largest singular value.
.TP 7
WORK   (workspace) REAL array, dimension at least
(9*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2),
where NLVL = max(0, INT(log_2 (N/(SMLSIZ+1))) + 1).
.TP 7
IWORK  (workspace) INTEGER array, dimension at least
(3*N*NLVL + 11*N)
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  The algorithm failed to compute an singular value while
working on the submatrix lying in rows and columns
INFO/(N+1) through MOD(INFO,N+1).
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Ren-Cang Li, Computer Science Division, University of
     California at Berkeley, USA
.br
   Osni Marques, LBNL/NERSC, USA
.br

