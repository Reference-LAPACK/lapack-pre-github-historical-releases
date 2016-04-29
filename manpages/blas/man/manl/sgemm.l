.TH  SGEMM 1 "November 2006" "BLAS routine" "BLAS routine" 
.SH NAME
SGEMM - one of the matrix-matrix operations   C := alpha*op( A )*op( B ) + beta*C,
.SH SYNOPSIS
.TP 67
SUBROUTINE SGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
.TP 67
.ti +4
REAL
ALPHA,BETA
.TP 67
.ti +4
INTEGER
K,LDA,LDB,LDC,M,N
.TP 67
.ti +4
CHARACTER
TRANSA,TRANSB
.TP 67
.ti +4
REAL
A(LDA,*),B(LDB,*),C(LDC,*)
.SH PURPOSE
SGEMM  performs one of the matrix-matrix operations

where  op( X ) is one of
.br

   op( X ) = X   or   op( X ) = X\(aq,
.br

alpha and beta are scalars, and A, B and C are matrices, with op( A )
an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.

.SH ARGUMENTS

TRANSA - CHARACTER*1.
On entry, TRANSA specifies the form of op( A ) to be used in
the matrix multiplication as follows:

TRANSA = \(aqN\(aq or \(aqn\(aq,  op( A ) = A.

TRANSA = \(aqT\(aq or \(aqt\(aq,  op( A ) = A\(aq.

TRANSA = \(aqC\(aq or \(aqc\(aq,  op( A ) = A\(aq.

Unchanged on exit.

TRANSB - CHARACTER*1.
On entry, TRANSB specifies the form of op( B ) to be used in
the matrix multiplication as follows:

TRANSB = \(aqN\(aq or \(aqn\(aq,  op( B ) = B.

TRANSB = \(aqT\(aq or \(aqt\(aq,  op( B ) = B\(aq.

TRANSB = \(aqC\(aq or \(aqc\(aq,  op( B ) = B\(aq.

Unchanged on exit.
.TP 7
M      - INTEGER.
On entry,  M  specifies  the number  of rows  of the  matrix
op( A )  and of the  matrix  C.  M  must  be at least  zero.
Unchanged on exit.
.TP 7
N      - INTEGER.
On entry,  N  specifies the number  of columns of the matrix
op( B ) and the number of columns of the matrix C. N must be
at least zero.
Unchanged on exit.
.TP 7
K      - INTEGER.
On entry,  K  specifies  the number of columns of the matrix
op( A ) and the number of rows of the matrix op( B ). K must
be at least  zero.
Unchanged on exit.
.TP 7
ALPHA  - REAL            .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - REAL             array of DIMENSION ( LDA, ka ), where ka is
k  when  TRANSA = \(aqN\(aq or \(aqn\(aq,  and is  m  otherwise.
Before entry with  TRANSA = \(aqN\(aq or \(aqn\(aq,  the leading  m by k
part of the array  A  must contain the matrix  A,  otherwise
the leading  k by m  part of the array  A  must contain  the
matrix A.
Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. When  TRANSA = \(aqN\(aq or \(aqn\(aq then
LDA must be at least  max( 1, m ), otherwise  LDA must be at
least  max( 1, k ).
Unchanged on exit.
.TP 7
B      - REAL             array of DIMENSION ( LDB, kb ), where kb is
n  when  TRANSB = \(aqN\(aq or \(aqn\(aq,  and is  k  otherwise.
Before entry with  TRANSB = \(aqN\(aq or \(aqn\(aq,  the leading  k by n
part of the array  B  must contain the matrix  B,  otherwise
the leading  n by k  part of the array  B  must contain  the
matrix B.
Unchanged on exit.
.TP 7
LDB    - INTEGER.
On entry, LDB specifies the first dimension of B as declared
in the calling (sub) program. When  TRANSB = \(aqN\(aq or \(aqn\(aq then
LDB must be at least  max( 1, k ), otherwise  LDB must be at
least  max( 1, n ).
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
On exit, the array  C  is overwritten by the  m by n  matrix
( alpha*op( A )*op( B ) + beta*C ).
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
