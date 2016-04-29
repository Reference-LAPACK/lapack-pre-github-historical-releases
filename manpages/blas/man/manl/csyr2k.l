.TH  CSYR2K 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
CSYR2K - one of the symmetric rank 2k operations   C := alpha*A*B\(aq + alpha*B*A\(aq + beta*C,
.SH SYNOPSIS
.TP 63
SUBROUTINE CSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
.TP 63
.ti +4
COMPLEX
ALPHA,BETA
.TP 63
.ti +4
INTEGER
K,LDA,LDB,LDC,N
.TP 63
.ti +4
CHARACTER
TRANS,UPLO
.TP 63
.ti +4
COMPLEX
A(LDA,*),B(LDB,*),C(LDC,*)
.SH PURPOSE
CSYR2K  performs one of the symmetric rank 2k operations

or
.br

   C := alpha*A\(aq*B + alpha*B\(aq*A + beta*C,
.br

where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
and  A and B  are  n by k  matrices  in the  first  case  and  k by n
matrices in the second case.
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

TRANS = \(aqN\(aq or \(aqn\(aq    C := alpha*A*B\(aq + alpha*B*A\(aq +
beta*C.

TRANS = \(aqT\(aq or \(aqt\(aq    C := alpha*A\(aq*B + alpha*B\(aq*A +
beta*C.

Unchanged on exit.
.TP 7
N      - INTEGER.
On entry,  N specifies the order of the matrix C.  N must be
at least zero.
Unchanged on exit.
.TP 7
K      - INTEGER.
On entry with  TRANS = \(aqN\(aq or \(aqn\(aq,  K  specifies  the number
of  columns  of the  matrices  A and B,  and on  entry  with
TRANS = \(aqT\(aq or \(aqt\(aq,  K  specifies  the number of rows of the
matrices  A and B.  K must be at least zero.
Unchanged on exit.
.TP 7
ALPHA  - COMPLEX         .
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
B      - COMPLEX          array of DIMENSION ( LDB, kb ), where kb is
k  when  TRANS = \(aqN\(aq or \(aqn\(aq,  and is  n  otherwise.
Before entry with  TRANS = \(aqN\(aq or \(aqn\(aq,  the  leading  n by k
part of the array  B  must contain the matrix  B,  otherwise
the leading  k by n  part of the array  B  must contain  the
matrix B.
Unchanged on exit.
.TP 7
LDB    - INTEGER.
On entry, LDB specifies the first dimension of B as declared
in  the  calling  (sub)  program.   When  TRANS = \(aqN\(aq or \(aqn\(aq
then  LDB must be at least  max( 1, n ), otherwise  LDB must
be at least  max( 1, k ).
Unchanged on exit.
.TP 7
BETA   - COMPLEX         .
On entry, BETA specifies the scalar beta.
Unchanged on exit.
.TP 7
C      - COMPLEX          array of DIMENSION ( LDC, n ).
Before entry  with  UPLO = \(aqU\(aq or \(aqu\(aq,  the leading  n by n
upper triangular part of the array C must contain the upper
triangular part  of the  symmetric matrix  and the strictly
lower triangular part of C is not referenced.  On exit, the
upper triangular part of the array  C is overwritten by the
upper triangular part of the updated matrix.
Before entry  with  UPLO = \(aqL\(aq or \(aql\(aq,  the leading  n by n
lower triangular part of the array C must contain the lower
triangular part  of the  symmetric matrix  and the strictly
upper triangular part of C is not referenced.  On exit, the
lower triangular part of the array  C is overwritten by the
lower triangular part of the updated matrix.
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

.. External Functions ..

..
.. External Subroutines ..

..
