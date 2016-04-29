.TH  ZLAGTM 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
ZLAGTM - a matrix-vector product of the form   B := alpha * A * X + beta * B  where A is a tridiagonal matrix of order N, B and X are N by NRHS matrices, and alpha and beta are real scalars, each of which may be 0., 1., or -1
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAGTM(
TRANS, N, NRHS, ALPHA, DL, D, DU, X, LDX, BETA,
B, LDB )
.TP 19
.ti +4
CHARACTER
TRANS
.TP 19
.ti +4
INTEGER
LDB, LDX, N, NRHS
.TP 19
.ti +4
DOUBLE
PRECISION ALPHA, BETA
.TP 19
.ti +4
COMPLEX*16
B( LDB, * ), D( * ), DL( * ), DU( * ),
X( LDX, * )
.SH PURPOSE
ZLAGTM performs a matrix-vector product of the form

.SH ARGUMENTS
.TP 8
TRANS   (input) CHARACTER*1
Specifies the operation applied to A.
= \(aqN\(aq:  No transpose, B := alpha * A * X + beta * B
.br
= \(aqT\(aq:  Transpose,    B := alpha * A**T * X + beta * B
.br
= \(aqC\(aq:  Conjugate transpose, B := alpha * A**H * X + beta * B
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices X and B.
.TP 8
ALPHA   (input) DOUBLE PRECISION
The scalar alpha.  ALPHA must be 0., 1., or -1.; otherwise,
it is assumed to be 0.
.TP 8
DL      (input) COMPLEX*16 array, dimension (N-1)
The (n-1) sub-diagonal elements of T.
.TP 8
D       (input) COMPLEX*16 array, dimension (N)
The diagonal elements of T.
.TP 8
DU      (input) COMPLEX*16 array, dimension (N-1)
The (n-1) super-diagonal elements of T.
.TP 8
X       (input) COMPLEX*16 array, dimension (LDX,NRHS)
The N by NRHS matrix X.
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(N,1).
.TP 8
BETA    (input) DOUBLE PRECISION
The scalar beta.  BETA must be 0., 1., or -1.; otherwise,
it is assumed to be 1.
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
On entry, the N by NRHS matrix B.
On exit, B is overwritten by the matrix expression
B := alpha * A * X + beta * B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(N,1).
