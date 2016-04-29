.TH  CHERK 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CHERK - one of the hermitian rank k operations   C := alpha*A*conjg( A\(aq ) + beta*C,
.SH SYNOPSIS
.TP 56
SUBROUTINE CHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
.TP 56
.ti +4
REAL
ALPHA,BETA
.TP 56
.ti +4
INTEGER
K,LDA,LDC,N
.TP 56
.ti +4
CHARACTER
TRANS,UPLO
.TP 56
.ti +4
COMPLEX
A(LDA,*),C(LDC,*)
.SH PURPOSE
CHERK  performs one of the hermitian rank k operations

or
.br

   C := alpha*conjg( A\(aq )*A + beta*C,
.br

where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
matrix and  A  is an  n by k  matrix in the  first case and a  k by n
matrix in the second case.
.br

.SH ARGUMENTS
.TP 7
UPLO   - CHARACTER*1.
On  entry,   UPLO  specifies  whether  the  upper  or  lower
triangular  part  of the  array  C  is to be  referenced  as
follows:

UPLO = \(aqU\(aq or \(aqu\(aq   Only the  upper triangular part of  C
is to be referenced.

UPLO = \(aqL\(aq or \(aql\(aq   Only the  lower triangular part of  C
is to be referenced.

Unchanged on exit.
.TP 7
TRANS  - CHARACTER*1.
On entry,  TRANS  specifies the operation to be performed as
follows:

TRANS = \(aqN\(aq or \(aqn\(aq   C := alpha*A*conjg( A\(aq ) + beta*C.

TRANS = \(aqC\(aq or \(aqc\(aq   C := alpha*conjg( A\(aq )*A + beta*C.

Unchanged on exit.
.TP 7
N      - INTEGER.
On entry,  N specifies the order of the matrix C.  N must be
at least zero.
Unchanged on exit.
.TP 7
K      - INTEGER.
On entry with  TRANS = \(aqN\(aq or \(aqn\(aq,  K  specifies  the number
of  columns   of  the   matrix   A,   and  on   entry   with
TRANS = \(aqC\(aq or \(aqc\(aq,  K  specifies  the number of rows of the
matrix A.  K must be at least zero.
Unchanged on exit.
.TP 7
ALPHA  - REAL            .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - COMPLEX          array of DIMENSION ( LDA, ka ), where ka is
k  when  TRANS = \(aqN\(aq or \(aqn\(aq,  and is  n  otherwise.
Before entry with  TRANS = \(aqN\(aq or \(aqn\(aq,  the  leading  n by k
part of the array  A  must contain the matrix  A,  otherwise
the leading  k by n  part of the array  A  must contain  the
matrix A.
Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in  the  calling  (sub)  program.   When  TRANS = \(aqN\(aq or \(aqn\(aq
then  LDA must be at least  max( 1, n ), otherwise  LDA must
be at least  max( 1, k ).
Unchanged on exit.
.TP 7
BETA   - REAL            .
On entry, BETA specifies the scalar beta.
Unchanged on exit.
.TP 7
C      - COMPLEX          array of DIMENSION ( LDC, n ).
Before entry  with  UPLO = \(aqU\(aq or \(aqu\(aq,  the leading  n by n
upper triangular part of the array C must contain the upper
triangular part  of the  hermitian matrix  and the strictly
lower triangular part of C is not referenced.  On exit, the
upper triangular part of the array  C is overwritten by the
upper triangular part of the updated matrix.
Before entry  with  UPLO = \(aqL\(aq or \(aql\(aq,  the leading  n by n
lower triangular part of the array C must contain the lower
triangular part  of the  hermitian matrix  and the strictly
upper triangular part of C is not referenced.  On exit, the
lower triangular part of the array  C is overwritten by the
lower triangular part of the updated matrix.
Note that the imaginary parts of the diagonal elements need
not be set,  they are assumed to be zero,  and on exit they
are set to zero.
.TP 7
LDC    - INTEGER.
On entry, LDC specifies the first dimension of C as declared
in  the  calling  (sub)  program.   LDC  must  be  at  least
max( 1, n ).
Unchanged on exit.

Level 3 Blas routine.

-- Written on 8-February-1989.
Jack Dongarra, Argonne National Laboratory.
Iain Duff, AERE Harwell.
Jeremy Du Croz, Numerical Algorithms Group Ltd.
Sven Hammarling, Numerical Algorithms Group Ltd.

-- Modified 8-Nov-93 to set C(J,J) to REAL( C(J,J) ) when BETA = 1.
Ed Anderson, Cray Research Inc.

.. External Functions ..

..
.. External Subroutines ..

..
