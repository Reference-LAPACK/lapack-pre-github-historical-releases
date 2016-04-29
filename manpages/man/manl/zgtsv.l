.TH  ZGTSV 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZGTSV - the equation   A*X = B,
.SH SYNOPSIS
.TP 18
SUBROUTINE ZGTSV(
N, NRHS, DL, D, DU, B, LDB, INFO )
.TP 18
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 18
.ti +4
COMPLEX*16
B( LDB, * ), D( * ), DL( * ), DU( * )
.SH PURPOSE
ZGTSV  solves the equation

where A is an N-by-N tridiagonal matrix, by Gaussian elimination with
partial pivoting.
.br

Note that the equation  A\(aq*X = B  may be solved by interchanging the
order of the arguments DU and DL.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
DL      (input/output) COMPLEX*16 array, dimension (N-1)
On entry, DL must contain the (n-1) subdiagonal elements of
A.
On exit, DL is overwritten by the (n-2) elements of the
second superdiagonal of the upper triangular matrix U from
the LU factorization of A, in DL(1), ..., DL(n-2).
.TP 8
D       (input/output) COMPLEX*16 array, dimension (N)
On entry, D must contain the diagonal elements of A.
On exit, D is overwritten by the n diagonal elements of U.
.TP 8
DU      (input/output) COMPLEX*16 array, dimension (N-1)
On entry, DU must contain the (n-1) superdiagonal elements
of A.
On exit, DU is overwritten by the (n-1) elements of the first
superdiagonal of U.
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
On entry, the N-by-NRHS right hand side matrix B.
On exit, if INFO = 0, the N-by-NRHS solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, U(i,i) is exactly zero, and the solution
has not been computed.  The factorization has not been
completed unless i = N.
