.TH  ZTRTRI 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZTRTRI - the inverse of a complex upper or lower triangular matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE ZTRTRI(
UPLO, DIAG, N, A, LDA, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * )
.SH PURPOSE
ZTRTRI computes the inverse of a complex upper or lower triangular
matrix A.

This is the Level 3 BLAS version of the algorithm.
.br

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
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the triangular matrix A.  If UPLO = \(aqU\(aq, the
leading N-by-N upper triangular part of the array A contains
the upper triangular matrix, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading N-by-N lower triangular part of the array A contains
the lower triangular matrix, and the strictly upper
triangular part of A is not referenced.  If DIAG = \(aqU\(aq, the
diagonal elements of A are also not referenced and are
assumed to be 1.
On exit, the (triangular) inverse of the original matrix, in
the same storage format.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = i, A(i,i) is exactly zero.  The triangular
matrix is singular and its inverse can not be computed.
