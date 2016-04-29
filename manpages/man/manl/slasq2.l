.TH  SLASQ2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLASQ2 - all the eigenvalues of the symmetric positive definite tridiagonal matrix associated with the qd array Z to high relative accuracy are computed to high relative accuracy, in the absence of denormalization, underflow and overflow
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASQ2(
N, Z, INFO )
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
REAL
Z( * )
.SH PURPOSE
SLASQ2 computes all the eigenvalues of the symmetric positive 
definite tridiagonal matrix associated with the qd array Z to high
relative accuracy are computed to high relative accuracy, in the
absence of denormalization, underflow and overflow.

To see the relation of Z to the tridiagonal matrix, let L be a
unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
let U be an upper bidiagonal matrix with 1\(aqs above and diagonal
Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
symmetric tridiagonal to which it is similar.
.br

Note : SLASQ2 defines a logical variable, IEEE, which is true
on machines which follow ieee-754 floating-point standard in their
handling of infinities and NaNs, and false otherwise. This variable
is passed to SLAZQ3.
.br

.SH ARGUMENTS
.TP 6
N     (input) INTEGER
The number of rows and columns in the matrix. N >= 0.
.TP 6
Z     (workspace) REAL array, dimension (4*N)
On entry Z holds the qd array. On exit, entries 1 to N hold
the eigenvalues in decreasing order, Z( 2*N+1 ) holds the
trace, and Z( 2*N+2 ) holds the sum of the eigenvalues. If
N > 2, then Z( 2*N+3 ) holds the iteration count, Z( 2*N+4 )
holds NDIVS/NIN^2, and Z( 2*N+5 ) holds the percentage of
shifts that failed.
.TP 6
INFO  (output) INTEGER
= 0: successful exit
.br
< 0: if the i-th argument is a scalar and had an illegal
value, then INFO = -i, if the i-th argument is an
array and the j-entry had an illegal value, then
INFO = -(i*100+j)
> 0: the algorithm failed
= 1, a split was marked by a positive value in E
= 2, current block of Z not diagonalized after 30*N
iterations (in inner while loop)
= 3, termination criterion of outer while loop not met 
(program created more than N unreduced blocks)
.SH FURTHER DETAILS
The shifts are accumulated in SIGMA. Iteration count is in ITER.
Ping-pong is controlled by PP (alternates between 0 and 1).

