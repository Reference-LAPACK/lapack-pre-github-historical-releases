.TH  ZLAED7 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
ZLAED7 - the updated eigensystem of a diagonal matrix after modification by a rank-one symmetric matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLAED7(
N, CUTPNT, QSIZ, TLVLS, CURLVL, CURPBM, D, Q,
LDQ, RHO, INDXQ, QSTORE, QPTR, PRMPTR, PERM,
GIVPTR, GIVCOL, GIVNUM, WORK, RWORK, IWORK,
INFO )
.TP 19
.ti +4
INTEGER
CURLVL, CURPBM, CUTPNT, INFO, LDQ, N, QSIZ,
TLVLS
.TP 19
.ti +4
DOUBLE
PRECISION RHO
.TP 19
.ti +4
INTEGER
GIVCOL( 2, * ), GIVPTR( * ), INDXQ( * ),
IWORK( * ), PERM( * ), PRMPTR( * ), QPTR( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), GIVNUM( 2, * ), QSTORE( * ), RWORK( * )
.TP 19
.ti +4
COMPLEX*16
Q( LDQ, * ), WORK( * )
.SH PURPOSE
ZLAED7 computes the updated eigensystem of a diagonal
matrix after modification by a rank-one symmetric matrix. This
routine is used only for the eigenproblem which requires all
eigenvalues and optionally eigenvectors of a dense or banded
Hermitian matrix that has been reduced to tridiagonal form.

  T = Q(in) ( D(in) + RHO * Z*Z\(aq ) Q\(aq(in) = Q(out) * D(out) * Q\(aq(out)

  where Z = Q\(aqu, u is a vector of length N with ones in the
  CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
.br

   The eigenvectors of the original matrix are stored in Q, and the
   eigenvalues are in D.  The algorithm consists of three stages:

      The first stage consists of deflating the size of the problem
      when there are multiple eigenvalues or if there is a zero in
      the Z vector.  For each such occurence the dimension of the
      secular equation problem is reduced by one.  This stage is
      performed by the routine DLAED2.
.br

      The second stage consists of calculating the updated
      eigenvalues. This is done by finding the roots of the secular
      equation via the routine DLAED4 (as called by SLAED3).
      This routine also calculates the eigenvectors of the current
      problem.
.br

      The final stage consists of computing the updated eigenvectors
      directly using the updated eigenvalues.  The eigenvectors for
      the current problem are multiplied with the eigenvectors from
      the overall problem.
.br

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
The dimension of the symmetric tridiagonal matrix.  N >= 0.

CUTPNT (input) INTEGER
Contains the location of the last eigenvalue in the leading
sub-matrix.  min(1,N) <= CUTPNT <= N.
.TP 7
QSIZ   (input) INTEGER
The dimension of the unitary matrix used to reduce
the full matrix to tridiagonal form.  QSIZ >= N.
.TP 7
TLVLS  (input) INTEGER
The total number of merging levels in the overall divide and
conquer tree.

CURLVL (input) INTEGER
The current level in the overall merge routine,
0 <= curlvl <= tlvls.

CURPBM (input) INTEGER
The current problem in the current level in the overall
merge routine (counting from upper left to lower right).
.TP 7
D      (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the eigenvalues of the rank-1-perturbed matrix.
On exit, the eigenvalues of the repaired matrix.
.TP 7
Q      (input/output) COMPLEX*16 array, dimension (LDQ,N)
On entry, the eigenvectors of the rank-1-perturbed matrix.
On exit, the eigenvectors of the repaired tridiagonal matrix.
.TP 7
LDQ    (input) INTEGER
The leading dimension of the array Q.  LDQ >= max(1,N).
.TP 7
RHO    (input) DOUBLE PRECISION
Contains the subdiagonal element used to create the rank-1
modification.
.TP 7
INDXQ  (output) INTEGER array, dimension (N)
This contains the permutation which will reintegrate the
subproblem just solved back into sorted order,
ie. D( INDXQ( I = 1, N ) ) will be in ascending order.
.TP 7
IWORK  (workspace) INTEGER array, dimension (4*N)
.TP 7
RWORK  (workspace) DOUBLE PRECISION array,
dimension (3*N+2*QSIZ*N)
.TP 7
WORK   (workspace) COMPLEX*16 array, dimension (QSIZ*N)

QSTORE (input/output) DOUBLE PRECISION array, dimension (N**2+1)
Stores eigenvectors of submatrices encountered during
divide and conquer, packed together. QPTR points to
beginning of the submatrices.
.TP 7
QPTR   (input/output) INTEGER array, dimension (N+2)
List of indices pointing to beginning of submatrices stored
in QSTORE. The submatrices are numbered starting at the
bottom left of the divide and conquer tree, from left to
right and bottom to top.

PRMPTR (input) INTEGER array, dimension (N lg N)
Contains a list of pointers which indicate where in PERM a
level\(aqs permutation is stored.  PRMPTR(i+1) - PRMPTR(i)
indicates the size of the permutation and also the size of
the full, non-deflated problem.
.TP 7
PERM   (input) INTEGER array, dimension (N lg N)
Contains the permutations (from deflation and sorting) to be
applied to each eigenblock.

GIVPTR (input) INTEGER array, dimension (N lg N)
Contains a list of pointers which indicate where in GIVCOL a
level\(aqs Givens rotations are stored.  GIVPTR(i+1) - GIVPTR(i)
indicates the number of Givens rotations.

GIVCOL (input) INTEGER array, dimension (2, N lg N)
Each pair of numbers indicates a pair of columns to take place
in a Givens rotation.

GIVNUM (input) DOUBLE PRECISION array, dimension (2, N lg N)
Each number indicates the S value to be used in the
corresponding Givens rotation.
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = 1, an eigenvalue did not converge
