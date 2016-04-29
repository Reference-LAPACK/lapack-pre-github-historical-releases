.TH  DLASR 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
DLASR - a sequence of plane rotations to a real matrix A,
.SH SYNOPSIS
.TP 18
SUBROUTINE DLASR(
SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
.TP 18
.ti +4
CHARACTER
DIRECT, PIVOT, SIDE
.TP 18
.ti +4
INTEGER
LDA, M, N
.TP 18
.ti +4
DOUBLE
PRECISION A( LDA, * ), C( * ), S( * )
.SH PURPOSE
DLASR applies a sequence of plane rotations to a real matrix A,
from either the left or the right.
.br

.br
When SIDE = \(aqL\(aq, the transformation takes the form
.br

.br
   A := P*A
.br

.br
and when SIDE = \(aqR\(aq, the transformation takes the form
.br

.br
   A := A*P**T
.br

.br
where P is an orthogonal matrix consisting of a sequence of z plane
rotations, with z = M when SIDE = \(aqL\(aq and z = N when SIDE = \(aqR\(aq,
and P**T is the transpose of P.
.br

.br
When DIRECT = \(aqF\(aq (Forward sequence), then
.br

.br
   P = P(z-1) * ... * P(2) * P(1)
.br

.br
and when DIRECT = \(aqB\(aq (Backward sequence), then
.br

.br
   P = P(1) * P(2) * ... * P(z-1)
.br

.br
where P(k) is a plane rotation matrix defined by the 2-by-2 rotation

.br
   R(k) = (  c(k)  s(k) )
.br
        = ( -s(k)  c(k) ).
.br

.br
When PIVOT = \(aqV\(aq (Variable pivot), the rotation is performed
for the plane (k,k+1), i.e., P(k) has the form
.br

.br
   P(k) = (  1                                            )
          (       ...                                     )
          (              1                                )
          (                   c(k)  s(k)                  )
          (                  -s(k)  c(k)                  )
          (                                1              )
          (                                     ...       )
          (                                            1  )

.br
where R(k) appears as a rank-2 modification to the identity matrix in
rows and columns k and k+1.
.br

.br
When PIVOT = \(aqT\(aq (Top pivot), the rotation is performed for the
plane (1,k+1), so P(k) has the form
.br

.br
   P(k) = (  c(k)                    s(k)                 )
          (         1                                     )
          (              ...                              )
          (                     1                         )
          ( -s(k)                    c(k)                 )
          (                                 1             )
          (                                      ...      )
          (                                             1 )

.br
where R(k) appears in rows and columns 1 and k+1.
.br

.br
Similarly, when PIVOT = \(aqB\(aq (Bottom pivot), the rotation is
performed for the plane (k,z), giving P(k) the form
.br

.br
   P(k) = ( 1                                             )
          (      ...                                      )
          (             1                                 )
          (                  c(k)                    s(k) )
          (                         1                     )
          (                              ...              )
          (                                     1         )
          (                 -s(k)                    c(k) )

.br
where R(k) appears in rows and columns k and z.  The rotations are
performed without ever forming P(k) explicitly.
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
Specifies whether the plane rotation matrix P is applied to
A on the left or the right.
= \(aqL\(aq:  Left, compute A := P*A
.br
= \(aqR\(aq:  Right, compute A:= A*P**T
.TP 8
PIVOT   (input) CHARACTER*1
Specifies the plane for which P(k) is a plane rotation
matrix.
= \(aqV\(aq:  Variable pivot, the plane (k,k+1)
.br
= \(aqT\(aq:  Top pivot, the plane (1,k+1)
.br
= \(aqB\(aq:  Bottom pivot, the plane (k,z)
.TP 8
DIRECT  (input) CHARACTER*1
Specifies whether P is a forward or backward sequence of
plane rotations.
= \(aqF\(aq:  Forward, P = P(z-1)*...*P(2)*P(1)
.br
= \(aqB\(aq:  Backward, P = P(1)*P(2)*...*P(z-1)
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  If m <= 1, an immediate
return is effected.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  If n <= 1, an
immediate return is effected.
.TP 8
C       (input) DOUBLE PRECISION array, dimension
(M-1) if SIDE = \(aqL\(aq
(N-1) if SIDE = \(aqR\(aq
The cosines c(k) of the plane rotations.
.TP 8
S       (input) DOUBLE PRECISION array, dimension
(M-1) if SIDE = \(aqL\(aq
(N-1) if SIDE = \(aqR\(aq
The sines s(k) of the plane rotations.  The 2-by-2 plane
rotation part of the matrix P(k), R(k), has the form
R(k) = (  c(k)  s(k) )
( -s(k)  c(k) ).
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
The M-by-N matrix A.  On exit, A is overwritten by P*A if
SIDE = \(aqR\(aq or by A*P**T if SIDE = \(aqL\(aq.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
