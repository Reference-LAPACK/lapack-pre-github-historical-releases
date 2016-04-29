.TH  DLARZT 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DLARZT - the triangular factor T of a real block reflector H of order > n, which is defined as a product of k elementary reflectors
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARZT(
DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
.TP 19
.ti +4
CHARACTER
DIRECT, STOREV
.TP 19
.ti +4
INTEGER
K, LDT, LDV, N
.TP 19
.ti +4
DOUBLE
PRECISION T( LDT, * ), TAU( * ), V( LDV, * )
.SH PURPOSE
DLARZT forms the triangular factor T of a real block reflector
H of order > n, which is defined as a product of k elementary
reflectors.

If DIRECT = \(aqF\(aq, H = H(1) H(2) . . . H(k) and T is upper triangular;

If DIRECT = \(aqB\(aq, H = H(k) . . . H(2) H(1) and T is lower triangular.

If STOREV = \(aqC\(aq, the vector which defines the elementary reflector
H(i) is stored in the i-th column of the array V, and
.br

   H  =  I - V * T * V\(aq
.br

If STOREV = \(aqR\(aq, the vector which defines the elementary reflector
H(i) is stored in the i-th row of the array V, and
.br

   H  =  I - V\(aq * T * V
.br

Currently, only STOREV = \(aqR\(aq and DIRECT = \(aqB\(aq are supported.

.SH ARGUMENTS
.TP 8
DIRECT  (input) CHARACTER*1
Specifies the order in which the elementary reflectors are
multiplied to form the block reflector:
.br
= \(aqF\(aq: H = H(1) H(2) . . . H(k) (Forward, not supported yet)
.br
= \(aqB\(aq: H = H(k) . . . H(2) H(1) (Backward)
.TP 8
STOREV  (input) CHARACTER*1
Specifies how the vectors which define the elementary
reflectors are stored (see also Further Details):
.br
= \(aqR\(aq: rowwise
.TP 8
N       (input) INTEGER
The order of the block reflector H. N >= 0.
.TP 8
K       (input) INTEGER
The order of the triangular factor T (= the number of
elementary reflectors). K >= 1.
.TP 8
V       (input/output) DOUBLE PRECISION array, dimension
(LDV,K) if STOREV = \(aqC\(aq
(LDV,N) if STOREV = \(aqR\(aq
The matrix V. See further details.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V.
If STOREV = \(aqC\(aq, LDV >= max(1,N); if STOREV = \(aqR\(aq, LDV >= K.
.TP 8
TAU     (input) DOUBLE PRECISION array, dimension (K)
TAU(i) must contain the scalar factor of the elementary
reflector H(i).
.TP 8
T       (output) DOUBLE PRECISION array, dimension (LDT,K)
The k by k triangular factor T of the block reflector.
If DIRECT = \(aqF\(aq, T is upper triangular; if DIRECT = \(aqB\(aq, T is
lower triangular. The rest of the array is not used.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= K.
.SH FURTHER DETAILS
Based on contributions by
.br
  A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA

The shape of the matrix V and the storage of the vectors which define
the H(i) is best illustrated by the following example with n = 5 and
k = 3. The elements equal to 1 are not stored; the corresponding
array elements are modified but restored on exit. The rest of the
array is not used.
.br

DIRECT = \(aqF\(aq and STOREV = \(aqC\(aq:         DIRECT = \(aqF\(aq and STOREV = \(aqR\(aq:

                                            ______V_____
.br
       ( v1 v2 v3 )                        /            \
       ( v1 v2 v3 )                      ( v1 v1 v1 v1 v1 . . . . 1 )
   V = ( v1 v2 v3 )                      ( v2 v2 v2 v2 v2 . . . 1   )
       ( v1 v2 v3 )                      ( v3 v3 v3 v3 v3 . . 1     )
       ( v1 v2 v3 )
.br
          .  .  .
.br
          .  .  .
.br
          1  .  .
.br
             1  .
.br
                1
.br

DIRECT = \(aqB\(aq and STOREV = \(aqC\(aq:         DIRECT = \(aqB\(aq and STOREV = \(aqR\(aq:

                                                      ______V_____
          1                                          /            \
          .  1                           ( 1 . . . . v1 v1 v1 v1 v1 )
          .  .  1                        ( . 1 . . . v2 v2 v2 v2 v2 )
          .  .  .                        ( . . 1 . . v3 v3 v3 v3 v3 )
          .  .  .
.br
       ( v1 v2 v3 )
.br
       ( v1 v2 v3 )
.br
   V = ( v1 v2 v3 )
.br
       ( v1 v2 v3 )
.br
       ( v1 v2 v3 )
.br

