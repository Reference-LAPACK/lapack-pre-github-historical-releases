.TH  DTRSYL 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DTRSYL - the real Sylvester matrix equation
.SH SYNOPSIS
.TP 19
SUBROUTINE DTRSYL(
TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,
LDC, SCALE, INFO )
.TP 19
.ti +4
CHARACTER
TRANA, TRANB
.TP 19
.ti +4
INTEGER
INFO, ISGN, LDA, LDB, LDC, M, N
.TP 19
.ti +4
DOUBLE
PRECISION SCALE
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), C( LDC, * )
.SH PURPOSE
DTRSYL solves the real Sylvester matrix equation:

   op(A)*X + X*op(B) = scale*C or
.br
   op(A)*X - X*op(B) = scale*C,
.br

where op(A) = A or A**T, and  A and B are both upper quasi-
triangular. A is M-by-M and B is N-by-N; the right hand side C and
the solution X are M-by-N; and scale is an output scale factor, set
<= 1 to avoid overflow in X.
.br

A and B must be in Schur canonical form (as returned by DHSEQR), that
is, block upper triangular with 1-by-1 and 2-by-2 diagonal blocks;
each 2-by-2 diagonal block has its diagonal elements equal and its
off-diagonal elements of opposite sign.
.br

.SH ARGUMENTS
.TP 8
TRANA   (input) CHARACTER*1
Specifies the option op(A):
.br
= \(aqN\(aq: op(A) = A    (No transpose)
.br
= \(aqT\(aq: op(A) = A**T (Transpose)
.br
= \(aqC\(aq: op(A) = A**H (Conjugate transpose = Transpose)
.TP 8
TRANB   (input) CHARACTER*1
.br
Specifies the option op(B):
.br
= \(aqN\(aq: op(B) = B    (No transpose)
.br
= \(aqT\(aq: op(B) = B**T (Transpose)
.br
= \(aqC\(aq: op(B) = B**H (Conjugate transpose = Transpose)
.TP 8
ISGN    (input) INTEGER
.br
Specifies the sign in the equation:
.br
= +1: solve op(A)*X + X*op(B) = scale*C
.br
= -1: solve op(A)*X - X*op(B) = scale*C
.TP 8
M       (input) INTEGER
The order of the matrix A, and the number of rows in the
matrices X and C. M >= 0.
.TP 8
N       (input) INTEGER
The order of the matrix B, and the number of columns in the
matrices X and C. N >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,M)
The upper quasi-triangular matrix A, in Schur canonical form.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
B       (input) DOUBLE PRECISION array, dimension (LDB,N)
The upper quasi-triangular matrix B, in Schur canonical form.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
On entry, the M-by-N right hand side matrix C.
On exit, C is overwritten by the solution matrix X.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M)
.TP 8
SCALE   (output) DOUBLE PRECISION
The scale factor, scale, set <= 1 to avoid overflow in X.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
= 1: A and B have common or very close eigenvalues; perturbed
values were used to solve the equation (but the matrices
A and B are unchanged).
