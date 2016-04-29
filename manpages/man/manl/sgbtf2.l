.TH  SGBTF2 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SGBTF2 - an LU factorization of a real m-by-n band matrix A using partial pivoting with row interchanges
.SH SYNOPSIS
.TP 19
SUBROUTINE SGBTF2(
M, N, KL, KU, AB, LDAB, IPIV, INFO )
.TP 19
.ti +4
INTEGER
INFO, KL, KU, LDAB, M, N
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
REAL
AB( LDAB, * )
.SH PURPOSE
SGBTF2 computes an LU factorization of a real m-by-n band matrix A
using partial pivoting with row interchanges.

This is the unblocked version of the algorithm, calling Level 2 BLAS.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
KL      (input) INTEGER
The number of subdiagonals within the band of A.  KL >= 0.
.TP 8
KU      (input) INTEGER
The number of superdiagonals within the band of A.  KU >= 0.
.TP 8
AB      (input/output) REAL array, dimension (LDAB,N)
On entry, the matrix A in band storage, in rows KL+1 to
2*KL+KU+1; rows 1 to KL of the array need not be set.
The j-th column of A is stored in the j-th column of the
array AB as follows:
AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)

On exit, details of the factorization: U is stored as an
upper triangular band matrix with KL+KU superdiagonals in
rows 1 to KL+KU+1, and the multipliers used during the
factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
See below for further details.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
.TP 8
IPIV    (output) INTEGER array, dimension (min(M,N))
The pivot indices; for 1 <= i <= min(M,N), row i of the
matrix was interchanged with row IPIV(i).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
> 0: if INFO = +i, U(i,i) is exactly zero. The factorization
has been completed, but the factor U is exactly
singular, and division by zero will occur if it is used
to solve a system of equations.
.SH FURTHER DETAILS
The band storage scheme is illustrated by the following example, when
M = N = 6, KL = 2, KU = 1:
.br

On entry:                       On exit:
.br

    *    *    *    +    +    +       *    *    *   u14  u25  u36
    *    *    +    +    +    +       *    *   u13  u24  u35  u46
    *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
   a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
   a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
   a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *

Array elements marked * are not used by the routine; elements marked
+ need not be set on entry, but are required by the routine to store
elements of U, because of fill-in resulting from the row
.br
interchanges.
.br

