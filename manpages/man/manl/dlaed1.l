.TH  DLAED1 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DLAED1 - the updated eigensystem of a diagonal matrix after modification by a rank-one symmetric matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAED1(
N, D, Q, LDQ, INDXQ, RHO, CUTPNT, WORK, IWORK,
INFO )
.TP 19
.ti +4
INTEGER
CUTPNT, INFO, LDQ, N
.TP 19
.ti +4
DOUBLE
PRECISION RHO
.TP 19
.ti +4
INTEGER
INDXQ( * ), IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), Q( LDQ, * ), WORK( * )
.SH PURPOSE
DLAED1 computes the updated eigensystem of a diagonal
matrix after modification by a rank-one symmetric matrix.  This
routine is used only for the eigenproblem which requires all
eigenvalues and eigenvectors of a tridiagonal matrix.  DLAED7 handles
the case in which eigenvalues only or eigenvalues and eigenvectors
of a full symmetric matrix (which was reduced to tridiagonal form)
are desired.
.br

  T = Q(in) ( D(in) + RHO * Z*Z\(aq ) Q\(aq(in) = Q(out) * D(out) * Q\(aq(out)

   where Z = Q\(aqu, u is a vector of length N with ones in the
   CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.

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
      equation via the routine DLAED4 (as called by DLAED3).
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
.TP 7
D      (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the eigenvalues of the rank-1-perturbed matrix.
On exit, the eigenvalues of the repaired matrix.
.TP 7
Q      (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
On entry, the eigenvectors of the rank-1-perturbed matrix.
On exit, the eigenvectors of the repaired tridiagonal matrix.
.TP 7
LDQ    (input) INTEGER
The leading dimension of the array Q.  LDQ >= max(1,N).
.TP 7
INDXQ  (input/output) INTEGER array, dimension (N)
On entry, the permutation which separately sorts the two
subproblems in D into ascending order.
On exit, the permutation which will reintegrate the
subproblems back into sorted order,
i.e. D( INDXQ( I = 1, N ) ) will be in ascending order.
.TP 7
RHO    (input) DOUBLE PRECISION
The subdiagonal entry used to create the rank-1 modification.

CUTPNT (input) INTEGER
The location of the last eigenvalue in the leading sub-matrix.
min(1,N) <= CUTPNT <= N/2.
.TP 7
WORK   (workspace) DOUBLE PRECISION array, dimension (4*N + N**2)
.TP 7
IWORK  (workspace) INTEGER array, dimension (4*N)
.TP 7
INFO   (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = 1, an eigenvalue did not converge
.SH FURTHER DETAILS
Based on contributions by
.br
   Jeff Rutter, Computer Science Division, University of California
   at Berkeley, USA
.br
Modified by Francoise Tisseur, University of Tennessee.
.br

