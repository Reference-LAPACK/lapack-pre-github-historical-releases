.TH  SSYMM 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
SSYMM - one of the matrix-matrix operations   C := alpha*A*B + beta*C,
.SH SYNOPSIS
.TP 61
SUBROUTINE SSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
.TP 61
.ti +4
REAL
ALPHA,BETA
.TP 61
.ti +4
INTEGER
LDA,LDB,LDC,M,N
.TP 61
.ti +4
CHARACTER
SIDE,UPLO
.TP 61
.ti +4
REAL
A(LDA,*),B(LDB,*),C(LDC,*)
.SH PURPOSE
SSYMM  performs one of the matrix-matrix operations

or
.br

   C := alpha*B*A + beta*C,
.br

where alpha and beta are scalars,  A is a symmetric matrix and  B and
C are  m by n matrices.
.br

.SH ARGUMENTS
.TP 7
SIDE   - CHARACTER*1.
On entry,  SIDE  specifies whether  the  symmetric matrix  A
appears on the  left or right  in the  operation as follows:

SIDE = \(aqL\(aq or \(aql\(aq   C := alpha*A*B + beta*C,

SIDE = \(aqR\(aq or \(aqr\(aq   C := alpha*B*A + beta*C,

Unchanged on exit.
.TP 7
UPLO   - CHARACTER*1.
On  entry,   UPLO  specifies  whether  the  upper  or  lower
triangular  part  of  the  symmetric  matrix   A  is  to  be
referenced as follows:

UPLO = \(aqU\(aq or \(aqu\(aq   Only the upper triangular part of the
symmetric matrix is to be referenced.

UPLO = \(aqL\(aq or \(aql\(aq   Only the lower triangular part of the
symmetric matrix is to be referenced.

Unchanged on exit.
.TP 7
M      - INTEGER.
On entry,  M  specifies the number of rows of the matrix  C.
M  must be at least zero.
Unchanged on exit.
.TP 7
N      - INTEGER.
On entry, N specifies the number of columns of the matrix C.
N  must be at least zero.
Unchanged on exit.
.TP 7
ALPHA  - REAL            .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - REAL             array of DIMENSION ( LDA, ka ), where ka is
m  when  SIDE = \(aqL\(aq or \(aql\(aq  and is  n otherwise.
Before entry  with  SIDE = \(aqL\(aq or \(aql\(aq,  the  m by m  part of
the array  A  must contain the  symmetric matrix,  such that
when  UPLO = \(aqU\(aq or \(aqu\(aq, the leading m by m upper triangular
part of the array  A  must contain the upper triangular part
of the  symmetric matrix and the  strictly  lower triangular
part of  A  is not referenced,  and when  UPLO = \(aqL\(aq or \(aql\(aq,
the leading  m by m  lower triangular part  of the  array  A
must  contain  the  lower triangular part  of the  symmetric
matrix and the  strictly upper triangular part of  A  is not
referenced.
Before entry  with  SIDE = \(aqR\(aq or \(aqr\(aq,  the  n by n  part of
the array  A  must contain the  symmetric matrix,  such that
when  UPLO = \(aqU\(aq or \(aqu\(aq, the leading n by n upper triangular
part of the array  A  must contain the upper triangular part
of the  symmetric matrix and the  strictly  lower triangular
part of  A  is not referenced,  and when  UPLO = \(aqL\(aq or \(aql\(aq,
the leading  n by n  lower triangular part  of the  array  A
must  contain  the  lower triangular part  of the  symmetric
matrix and the  strictly upper triangular part of  A  is not
referenced.
Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program.  When  SIDE = \(aqL\(aq or \(aql\(aq  then
LDA must be at least  max( 1, m ), otherwise  LDA must be at
least  max( 1, n ).
Unchanged on exit.
.TP 7
B      - REAL             array of DIMENSION ( LDB, n ).
Before entry, the leading  m by n part of the array  B  must
contain the matrix B.
Unchanged on exit.
.TP 7
LDB    - INTEGER.
On entry, LDB specifies the first dimension of B as declared
in  the  calling  (sub)  program.   LDB  must  be  at  least
max( 1, m ).
Unchanged on exit.
.TP 7
BETA   - REAL            .
On entry,  BETA  specifies the scalar  beta.  When  BETA  is
supplied as zero then C need not be set on input.
Unchanged on exit.
.TP 7
C      - REAL             array of DIMENSION ( LDC, n ).
Before entry, the leading  m by n  part of the array  C must
contain the matrix  C,  except when  beta  is zero, in which
case C need not be set on entry.
On exit, the array  C  is overwritten by the  m by n updated
matrix.
.TP 7
LDC    - INTEGER.
On entry, LDC specifies the first dimension of C as declared
in  the  calling  (sub)  program.   LDC  must  be  at  least
max( 1, m ).
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
