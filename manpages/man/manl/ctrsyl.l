.TH  CTRSYL 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
CTRSYL - the complex Sylvester matrix equation
.SH SYNOPSIS
.TP 19
SUBROUTINE CTRSYL(
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
REAL
SCALE
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), C( LDC, * )
.SH PURPOSE
CTRSYL solves the complex Sylvester matrix equation:

   op(A)*X + X*op(B) = scale*C or
.br
   op(A)*X - X*op(B) = scale*C,
.br

where op(A) = A or A**H, and A and B are both upper triangular. A is
M-by-M and B is N-by-N; the right hand side C and the solution X are
M-by-N; and scale is an output scale factor, set <= 1 to avoid
overflow in X.
.br

.SH ARGUMENTS
.TP 8
TRANA   (input) CHARACTER*1
Specifies the option op(A):
.br
= \(aqN\(aq: op(A) = A    (No transpose)
.br
= \(aqC\(aq: op(A) = A**H (Conjugate transpose)
.TP 8
TRANB   (input) CHARACTER*1
.br
Specifies the option op(B):
.br
= \(aqN\(aq: op(B) = B    (No transpose)
.br
= \(aqC\(aq: op(B) = B**H (Conjugate transpose)
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
A       (input) COMPLEX array, dimension (LDA,M)
The upper triangular matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
B       (input) COMPLEX array, dimension (LDB,N)
The upper triangular matrix B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
C       (input/output) COMPLEX array, dimension (LDC,N)
On entry, the M-by-N right hand side matrix C.
On exit, C is overwritten by the solution matrix X.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M)
.TP 8
SCALE   (output) REAL
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
