.TH  CLASYF 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CLASYF - a partial factorization of a complex symmetric matrix A using the Bunch-Kaufman diagonal pivoting method
.SH SYNOPSIS
.TP 19
SUBROUTINE CLASYF(
UPLO, N, NB, KB, A, LDA, IPIV, W, LDW, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, KB, LDA, LDW, N, NB
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), W( LDW, * )
.SH PURPOSE
CLASYF computes a partial factorization of a complex symmetric matrix
A using the Bunch-Kaufman diagonal pivoting method. The partial
factorization has the form:
.br

A  =  ( I  U12 ) ( A11  0  ) (  I    0   )  if UPLO = \(aqU\(aq, or:
      ( 0  U22 ) (  0   D  ) ( U12\(aq U22\(aq )
.br

A  =  ( L11  0 ) ( D    0  ) ( L11\(aq L21\(aq )  if UPLO = \(aqL\(aq
      ( L21  I ) ( 0   A22 ) (  0    I   )
.br

where the order of D is at most NB. The actual order is returned in
the argument KB, and is either NB or NB-1, or N if N <= NB.
Note that U\(aq denotes the transpose of U.
.br

CLASYF is an auxiliary routine called by CSYTRF. It uses blocked code
(calling Level 3 BLAS) to update the submatrix A11 (if UPLO = \(aqU\(aq) or
A22 (if UPLO = \(aqL\(aq).
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is stored:
.br
= \(aqU\(aq:  Upper triangular
.br
= \(aqL\(aq:  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NB      (input) INTEGER
The maximum number of columns of the matrix A that should be
factored.  NB should be at least 2 to allow for 2-by-2 pivot
blocks.
.TP 8
KB      (output) INTEGER
The number of columns of A that were actually factored.
KB is either NB-1 or NB, or N if N <= NB.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = \(aqU\(aq, the leading
n-by-n upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = \(aqL\(aq, the
leading n-by-n lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.
On exit, A contains details of the partial factorization.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
IPIV    (output) INTEGER array, dimension (N)
Details of the interchanges and the block structure of D.
If UPLO = \(aqU\(aq, only the last KB elements of IPIV are set;
if UPLO = \(aqL\(aq, only the first KB elements are set.

If IPIV(k) > 0, then rows and columns k and IPIV(k) were
interchanged and D(k,k) is a 1-by-1 diagonal block.
If UPLO = \(aqU\(aq and IPIV(k) = IPIV(k-1) < 0, then rows and
columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
is a 2-by-2 diagonal block.  If UPLO = \(aqL\(aq and IPIV(k) =
IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were
interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
.TP 8
W       (workspace) COMPLEX array, dimension (LDW,NB)
.TP 8
LDW     (input) INTEGER
The leading dimension of the array W.  LDW >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
> 0: if INFO = k, D(k,k) is exactly zero.  The factorization
has been completed, but the block diagonal matrix D is
exactly singular.
