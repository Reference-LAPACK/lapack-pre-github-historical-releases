.TH  ZHBTRD 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZHBTRD - a complex Hermitian band matrix A to real symmetric tridiagonal form T by a unitary similarity transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE ZHBTRD(
VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,
WORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO, VECT
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, LDQ, N
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * )
.TP 19
.ti +4
COMPLEX*16
AB( LDAB, * ), Q( LDQ, * ), WORK( * )
.SH PURPOSE
ZHBTRD reduces a complex Hermitian band matrix A to real symmetric
tridiagonal form T by a unitary similarity transformation:
Q**H * A * Q = T.
.br

.SH ARGUMENTS
.TP 8
VECT    (input) CHARACTER*1
= \(aqN\(aq:  do not form Q;
.br
= \(aqV\(aq:  form Q;
.br
= \(aqU\(aq:  update a matrix X, by forming X*Q.
.TP 8
UPLO    (input) CHARACTER*1
.br
= \(aqU\(aq:  Upper triangle of A is stored;
.br
= \(aqL\(aq:  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = \(aqU\(aq,
or the number of subdiagonals if UPLO = \(aqL\(aq.  KD >= 0.
.TP 8
AB      (input/output) COMPLEX*16 array, dimension (LDAB,N)
On entry, the upper or lower triangle of the Hermitian band
matrix A, stored in the first KD+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = \(aqU\(aq, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = \(aqL\(aq, AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
On exit, the diagonal elements of AB are overwritten by the
diagonal elements of the tridiagonal matrix T; if KD > 0, the
elements on the first superdiagonal (if UPLO = \(aqU\(aq) or the
first subdiagonal (if UPLO = \(aqL\(aq) are overwritten by the
off-diagonal elements of T; the rest of AB is overwritten by
values generated during the reduction.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
D       (output) DOUBLE PRECISION array, dimension (N)
The diagonal elements of the tridiagonal matrix T.
.TP 8
E       (output) DOUBLE PRECISION array, dimension (N-1)
The off-diagonal elements of the tridiagonal matrix T:
E(i) = T(i,i+1) if UPLO = \(aqU\(aq; E(i) = T(i+1,i) if UPLO = \(aqL\(aq.
.TP 8
Q       (input/output) COMPLEX*16 array, dimension (LDQ,N)
On entry, if VECT = \(aqU\(aq, then Q must contain an N-by-N
matrix X; if VECT = \(aqN\(aq or \(aqV\(aq, then Q need not be set.

On exit:
if VECT = \(aqV\(aq, Q contains the N-by-N unitary matrix Q;
if VECT = \(aqU\(aq, Q contains the product X*Q;
if VECT = \(aqN\(aq, the array Q is not referenced.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.
LDQ >= 1, and LDQ >= N if VECT = \(aqV\(aq or \(aqU\(aq.
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
Modified by Linda Kaufman, Bell Labs.
.br

