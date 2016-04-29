.TH  SGEEVX 1 "November 2006" " LAPACK driver routine (version 3.1) " " LAPACK driver routine (version 3.1) " 
.SH NAME
SGEEVX - for an N-by-N real nonsymmetric matrix A, the eigenvalues and, optionally, the left and/or right eigenvectors
.SH SYNOPSIS
.TP 19
SUBROUTINE SGEEVX(
BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, WR, WI,
VL, LDVL, VR, LDVR, ILO, IHI, SCALE, ABNRM,
RCONDE, RCONDV, WORK, LWORK, IWORK, INFO )
.TP 19
.ti +4
CHARACTER
BALANC, JOBVL, JOBVR, SENSE
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LDVL, LDVR, LWORK, N
.TP 19
.ti +4
REAL
ABNRM
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
A( LDA, * ), RCONDE( * ), RCONDV( * ),
SCALE( * ), VL( LDVL, * ), VR( LDVR, * ),
WI( * ), WORK( * ), WR( * )
.SH PURPOSE
SGEEVX computes for an N-by-N real nonsymmetric matrix A, the
eigenvalues and, optionally, the left and/or right eigenvectors.

Optionally also, it computes a balancing transformation to improve
the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
(RCONDE), and reciprocal condition numbers for the right
.br
eigenvectors (RCONDV).
.br

The right eigenvector v(j) of A satisfies
.br
                 A * v(j) = lambda(j) * v(j)
.br
where lambda(j) is its eigenvalue.
.br
The left eigenvector u(j) of A satisfies
.br
              u(j)**H * A = lambda(j) * u(j)**H
.br
where u(j)**H denotes the conjugate transpose of u(j).
.br

The computed eigenvectors are normalized to have Euclidean norm
equal to 1 and largest component real.
.br

Balancing a matrix means permuting the rows and columns to make it
more nearly upper triangular, and applying a diagonal similarity
transformation D * A * D**(-1), where D is a diagonal matrix, to
make its rows and columns closer in norm and the condition numbers
of its eigenvalues and eigenvectors smaller.  The computed
reciprocal condition numbers correspond to the balanced matrix.
Permuting rows and columns will not change the condition numbers
(in exact arithmetic) but diagonal scaling will.  For further
explanation of balancing, see section 4.10.2 of the LAPACK
Users\(aq Guide.
.br

.SH ARGUMENTS
.TP 8
BALANC  (input) CHARACTER*1
Indicates how the input matrix should be diagonally scaled
and/or permuted to improve the conditioning of its
eigenvalues.
= \(aqN\(aq: Do not diagonally scale or permute;
.br
= \(aqP\(aq: Perform permutations to make the matrix more nearly
upper triangular. Do not diagonally scale;
= \(aqS\(aq: Diagonally scale the matrix, i.e. replace A by
D*A*D**(-1), where D is a diagonal matrix chosen
to make the rows and columns of A more equal in
norm. Do not permute;
= \(aqB\(aq: Both diagonally scale and permute A.

Computed reciprocal condition numbers will be for the matrix
after balancing and/or permuting. Permuting does not change
condition numbers (in exact arithmetic), but balancing does.
.TP 8
JOBVL   (input) CHARACTER*1
= \(aqN\(aq: left eigenvectors of A are not computed;
.br
= \(aqV\(aq: left eigenvectors of A are computed.
If SENSE = \(aqE\(aq or \(aqB\(aq, JOBVL must = \(aqV\(aq.
.TP 8
JOBVR   (input) CHARACTER*1
= \(aqN\(aq: right eigenvectors of A are not computed;
.br
= \(aqV\(aq: right eigenvectors of A are computed.
If SENSE = \(aqE\(aq or \(aqB\(aq, JOBVR must = \(aqV\(aq.
.TP 8
SENSE   (input) CHARACTER*1
Determines which reciprocal condition numbers are computed.
= \(aqN\(aq: None are computed;
.br
= \(aqE\(aq: Computed for eigenvalues only;
.br
= \(aqV\(aq: Computed for right eigenvectors only;
.br
= \(aqB\(aq: Computed for eigenvalues and right eigenvectors.

If SENSE = \(aqE\(aq or \(aqB\(aq, both left and right eigenvectors
must also be computed (JOBVL = \(aqV\(aq and JOBVR = \(aqV\(aq).
.TP 8
N       (input) INTEGER
The order of the matrix A. N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the N-by-N matrix A.
On exit, A has been overwritten.  If JOBVL = \(aqV\(aq or
JOBVR = \(aqV\(aq, A contains the real Schur form of the balanced
version of the input matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
WR      (output) REAL array, dimension (N)
WI      (output) REAL array, dimension (N)
WR and WI contain the real and imaginary parts,
respectively, of the computed eigenvalues.  Complex
conjugate pairs of eigenvalues will appear consecutively
with the eigenvalue having the positive imaginary part
first.
.TP 8
VL      (output) REAL array, dimension (LDVL,N)
If JOBVL = \(aqV\(aq, the left eigenvectors u(j) are stored one
after another in the columns of VL, in the same order
as their eigenvalues.
If JOBVL = \(aqN\(aq, VL is not referenced.
If the j-th eigenvalue is real, then u(j) = VL(:,j),
the j-th column of VL.
If the j-th and (j+1)-st eigenvalues form a complex
conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
.br
u(j+1) = VL(:,j) - i*VL(:,j+1).
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL.  LDVL >= 1; if
JOBVL = \(aqV\(aq, LDVL >= N.
.TP 8
VR      (output) REAL array, dimension (LDVR,N)
If JOBVR = \(aqV\(aq, the right eigenvectors v(j) are stored one
after another in the columns of VR, in the same order
as their eigenvalues.
If JOBVR = \(aqN\(aq, VR is not referenced.
If the j-th eigenvalue is real, then v(j) = VR(:,j),
the j-th column of VR.
If the j-th and (j+1)-st eigenvalues form a complex
conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
.br
v(j+1) = VR(:,j) - i*VR(:,j+1).
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.  LDVR >= 1, and if
JOBVR = \(aqV\(aq, LDVR >= N.
.TP 8
ILO     (output) INTEGER
IHI     (output) INTEGER
ILO and IHI are integer values determined when A was
balanced.  The balanced A(i,j) = 0 if I > J and 
J = 1,...,ILO-1 or I = IHI+1,...,N.
.TP 8
SCALE   (output) REAL array, dimension (N)
Details of the permutations and scaling factors applied
when balancing A.  If P(j) is the index of the row and column
interchanged with row and column j, and D(j) is the scaling
factor applied to row and column j, then
SCALE(J) = P(J),    for J = 1,...,ILO-1
= D(J),    for J = ILO,...,IHI
= P(J)     for J = IHI+1,...,N.
The order in which the interchanges are made is N to IHI+1,
then 1 to ILO-1.
.TP 8
ABNRM   (output) REAL
The one-norm of the balanced matrix (the maximum
of the sum of absolute values of elements of any column).
.TP 8
RCONDE  (output) REAL array, dimension (N)
RCONDE(j) is the reciprocal condition number of the j-th
eigenvalue.
.TP 8
RCONDV  (output) REAL array, dimension (N)
RCONDV(j) is the reciprocal condition number of the j-th
right eigenvector.
.TP 8
WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.   If SENSE = \(aqN\(aq or \(aqE\(aq,
LWORK >= max(1,2*N), and if JOBVL = \(aqV\(aq or JOBVR = \(aqV\(aq,
LWORK >= 3*N.  If SENSE = \(aqV\(aq or \(aqB\(aq, LWORK >= N*(N+6).
For good performance, LWORK must generally be larger.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
IWORK   (workspace) INTEGER array, dimension (2*N-2)
If SENSE = \(aqN\(aq or \(aqE\(aq, not referenced.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = i, the QR algorithm failed to compute all the
eigenvalues, and no eigenvectors or condition numbers
have been computed; elements 1:ILO-1 and i+1:N of WR
and WI contain eigenvalues which have converged.
