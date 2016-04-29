.TH  DTPTRI 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DTPTRI - the inverse of a real upper or lower triangular matrix A stored in packed format
.SH SYNOPSIS
.TP 19
SUBROUTINE DTPTRI(
UPLO, DIAG, N, AP, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, UPLO
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
DOUBLE
PRECISION AP( * )
.SH PURPOSE
DTPTRI computes the inverse of a real upper or lower triangular
matrix A stored in packed format.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= \(aqU\(aq:  A is upper triangular;
.br
= \(aqL\(aq:  A is lower triangular.
.TP 8
DIAG    (input) CHARACTER*1
.br
= \(aqN\(aq:  A is non-unit triangular;
.br
= \(aqU\(aq:  A is unit triangular.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
On entry, the upper or lower triangular matrix A, stored
columnwise in a linear array.  The j-th column of A is stored
in the array AP as follows:
if UPLO = \(aqU\(aq, AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = \(aqL\(aq, AP(i + (j-1)*((2*n-j)/2) = A(i,j) for j<=i<=n.
See below for further details.
On exit, the (triangular) inverse of the original matrix, in
the same packed storage format.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, A(i,i) is exactly zero.  The triangular
matrix is singular and its inverse can not be computed.
.SH FURTHER DETAILS
A triangular matrix A can be transferred to packed storage using one
of the following program segments:
.br

UPLO = \(aqU\(aq:                      UPLO = \(aqL\(aq:
.br

      JC = 1                           JC = 1
.br
      DO 2 J = 1, N                    DO 2 J = 1, N
.br
         DO 1 I = 1, J                    DO 1 I = J, N
.br
            AP(JC+I-1) = A(I,J)              AP(JC+I-J) = A(I,J)
    1    CONTINUE                    1    CONTINUE
.br
         JC = JC + J                      JC = JC + N - J + 1
    2 CONTINUE                       2 CONTINUE
.br

