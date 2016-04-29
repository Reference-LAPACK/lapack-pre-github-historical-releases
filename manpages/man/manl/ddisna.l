.TH  DDISNA 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DDISNA - the reciprocal condition numbers for the eigenvectors of a real symmetric or complex Hermitian matrix or for the left or right singular vectors of a general m-by-n matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE DDISNA(
JOB, M, N, D, SEP, INFO )
.TP 19
.ti +4
CHARACTER
JOB
.TP 19
.ti +4
INTEGER
INFO, M, N
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), SEP( * )
.SH PURPOSE
DDISNA computes the reciprocal condition numbers for the eigenvectors
of a real symmetric or complex Hermitian matrix or for the left or
right singular vectors of a general m-by-n matrix. The reciprocal
condition number is the \(aqgap\(aq between the corresponding eigenvalue or
singular value and the nearest other one.
.br

The bound on the error, measured by angle in radians, in the I-th
computed vector is given by
.br

       DLAMCH( \(aqE\(aq ) * ( ANORM / SEP( I ) )
.br

where ANORM = 2-norm(A) = max( abs( D(j) ) ).  SEP(I) is not allowed
to be smaller than DLAMCH( \(aqE\(aq )*ANORM in order to limit the size of
the error bound.
.br

DDISNA may also be used to compute error bounds for eigenvectors of
the generalized symmetric definite eigenproblem.
.br

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
Specifies for which problem the reciprocal condition numbers
should be computed:
.br
= \(aqE\(aq:  the eigenvectors of a symmetric/Hermitian matrix;
.br
= \(aqL\(aq:  the left singular vectors of a general matrix;
.br
= \(aqR\(aq:  the right singular vectors of a general matrix.
.TP 8
M       (input) INTEGER
The number of rows of the matrix. M >= 0.
.TP 8
N       (input) INTEGER
If JOB = \(aqL\(aq or \(aqR\(aq, the number of columns of the matrix,
in which case N >= 0. Ignored if JOB = \(aqE\(aq.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (M) if JOB = \(aqE\(aq
dimension (min(M,N)) if JOB = \(aqL\(aq or \(aqR\(aq
The eigenvalues (if JOB = \(aqE\(aq) or singular values (if JOB =
\(aqL\(aq or \(aqR\(aq) of the matrix, in either increasing or decreasing
order. If singular values, they must be non-negative.
.TP 8
SEP     (output) DOUBLE PRECISION array, dimension (M) if JOB = \(aqE\(aq
dimension (min(M,N)) if JOB = \(aqL\(aq or \(aqR\(aq
The reciprocal condition numbers of the vectors.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
