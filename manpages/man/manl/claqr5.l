.TH  CLAQR5 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME

.SH SYNOPSIS
.TP 19
SUBROUTINE CLAQR5(
WANTT, WANTZ, KACC22, N, KTOP, KBOT, NSHFTS, S,
H, LDH, ILOZ, IHIZ, Z, LDZ, V, LDV, U, LDU, NV,
WV, LDWV, NH, WH, LDWH )
.TP 19
.ti +4
INTEGER
IHIZ, ILOZ, KACC22, KBOT, KTOP, LDH, LDU, LDV,
LDWH, LDWV, LDZ, N, NH, NSHFTS, NV
.TP 19
.ti +4
LOGICAL
WANTT, WANTZ
.TP 19
.ti +4
COMPLEX
H( LDH, * ), S( * ), U( LDU, * ), V( LDV, * ),
WH( LDWH, * ), WV( LDWV, * ), Z( LDZ, * )
.TP 19
.ti +4
COMPLEX
ZERO, ONE
.TP 19
.ti +4
PARAMETER
( ZERO = ( 0.0e0, 0.0e0 ),
ONE = ( 1.0e0, 0.0e0 ) )
.TP 19
.ti +4
REAL
RZERO, RONE
.TP 19
.ti +4
PARAMETER
( RZERO = 0.0e0, RONE = 1.0e0 )
.TP 19
.ti +4
COMPLEX
ALPHA, BETA, CDUM, REFSUM
.TP 19
.ti +4
REAL
H11, H12, H21, H22, SAFMAX, SAFMIN, SCL,
SMLNUM, TST1, TST2, ULP
.TP 19
.ti +4
INTEGER
I2, I4, INCOL, J, J2, J4, JBOT, JCOL, JLEN,
JROW, JTOP, K, K1, KDU, KMS, KNZ, KRCOL, KZS,
M, M22, MBOT, MEND, MSTART, MTOP, NBMPS, NDCOL,
NS, NU
.TP 19
.ti +4
LOGICAL
ACCUM, BLK22, BMP22
.TP 19
.ti +4
REAL
SLAMCH
.TP 19
.ti +4
EXTERNAL
SLAMCH
.TP 19
.ti +4
INTRINSIC
ABS, AIMAG, CONJG, MAX, MIN, MOD, REAL
.TP 19
.ti +4
COMPLEX
VT( 3 )
.TP 19
.ti +4
EXTERNAL
CGEMM, CLACPY, CLAQR1, CLARFG, CLASET, CTRMM,
SLABAD
.TP 19
.ti +4
REAL
CABS1
.TP 19
.ti +4
CABS1(
CDUM ) = ABS( REAL( CDUM ) ) + ABS( AIMAG( CDUM ) )
.TP 19
.ti +4
IF(
NSHFTS.LT.2 )
RETURN
.TP 19
.ti +4
IF(
KTOP.GE.KBOT )
RETURN
.TP 19
.ti +4
NS
= NSHFTS - MOD( NSHFTS, 2 )
.TP 19
.ti +4
SAFMIN
= SLAMCH( \(aqSAFE MINIMUM\(aq )
.TP 19
.ti +4
SAFMAX
= RONE / SAFMIN
.TP 19
.ti +4
CALL
SLABAD( SAFMIN, SAFMAX )
.TP 19
.ti +4
ULP
= SLAMCH( \(aqPRECISION\(aq )
.TP 19
.ti +4
SMLNUM
= SAFMIN*( REAL( N ) / ULP )
.TP 19
.ti +4
ACCUM
= ( KACC22.EQ.1 ) .OR. ( KACC22.EQ.2 )
.TP 19
.ti +4
BLK22
= ( NS.GT.2 ) .AND. ( KACC22.EQ.2 )
.TP 19
.ti +4
IF(
KTOP+2.LE.KBOT )
H( KTOP+2, KTOP ) = ZERO
.TP 19
.ti +4
NBMPS
= NS / 2
.TP 19
.ti +4
KDU
= 6*NBMPS - 3
.TP 19
.ti +4
DO
210 INCOL = 3*( 1-NBMPS ) + KTOP - 1, KBOT - 2, 3*NBMPS - 2
.TP 19
.ti +4
NDCOL
= INCOL + KDU
.TP 19
.ti +4
IF(
ACCUM )
CALL CLASET( \(aqALL\(aq, KDU, KDU, ZERO, ONE, U, LDU )
.TP 19
.ti +4
DO
140 KRCOL = INCOL, MIN( INCOL+3*NBMPS-3, KBOT-2 )
.TP 19
.ti +4
MTOP
= MAX( 1, ( ( KTOP-1 )-KRCOL+2 ) / 3+1 )
.TP 19
.ti +4
MBOT
= MIN( NBMPS, ( KBOT-KRCOL ) / 3 )
.TP 19
.ti +4
M22
= MBOT + 1
.TP 19
.ti +4
BMP22
= ( MBOT.LT.NBMPS ) .AND. ( KRCOL+3*( M22-1 ) ).EQ.
( KBOT-2 )
.TP 19
.ti +4
DO
10 M = MTOP, MBOT
.TP 19
.ti +4
K
= KRCOL + 3*( M-1 )
.TP 19
.ti +4
IF(
K.EQ.KTOP-1 ) THEN
.TP 19
.ti +4
CALL
CLAQR1( 3, H( KTOP, KTOP ), LDH, S( 2*M-1 ),
S( 2*M ), V( 1, M ) )
.TP 19
.ti +4
ALPHA
= V( 1, M )
.TP 19
.ti +4
CALL
CLARFG( 3, ALPHA, V( 2, M ), 1, V( 1, M ) )
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
BETA
= H( K+1, K )
.TP 19
.ti +4
V(
2, M ) = H( K+2, K )
.TP 19
.ti +4
V(
3, M ) = H( K+3, K )
.TP 19
.ti +4
CALL
CLARFG( 3, BETA, V( 2, M ), 1, V( 1, M ) )
.TP 19
.ti +4
IF(
V( 1, M ).NE.ZERO .AND.
( V( 3, M ).NE.ZERO .OR. ( H( K+3,
K+1 ).EQ.ZERO .AND. H( K+3, K+2 ).EQ.ZERO ) ) )
THEN
.TP 19
.ti +4
H(
K+1, K ) = BETA
.TP 19
.ti +4
H(
K+2, K ) = ZERO
.TP 19
.ti +4
H(
K+3, K ) = ZERO
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
CALL
CLAQR1( 3, H( K+1, K+1 ), LDH, S( 2*M-1 ),
S( 2*M ), VT )
.TP 19
.ti +4
SCL
= CABS1( VT( 1 ) ) + CABS1( VT( 2 ) ) +
CABS1( VT( 3 ) )
.TP 19
.ti +4
IF(
SCL.NE.RZERO ) THEN
.TP 19
.ti +4
VT(
1 ) = VT( 1 ) / SCL
.TP 19
.ti +4
VT(
2 ) = VT( 2 ) / SCL
.TP 19
.ti +4
VT(
3 ) = VT( 3 ) / SCL
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
CABS1( H( K+1, K ) )*
( CABS1( VT( 2 ) )+CABS1( VT( 3 ) ) ).GT.ULP*
CABS1( VT( 1 ) )*( CABS1( H( K,
K ) )+CABS1( H( K+1, K+1 ) )+CABS1( H( K+2,
K+2 ) ) ) ) THEN
.TP 19
.ti +4
IF(
V( 2, M ).EQ.ZERO .AND. V( 3, M ).EQ.ZERO )
THEN
.TP 19
.ti +4
V(
1, M ) = ZERO
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
H(
K+1, K ) = BETA
.TP 19
.ti +4
H(
K+2, K ) = ZERO
.TP 19
.ti +4
H(
K+3, K ) = ZERO
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
ALPHA
= VT( 1 )
.TP 19
.ti +4
CALL
CLARFG( 3, ALPHA, VT( 2 ), 1, VT( 1 ) )
.TP 19
.ti +4
REFSUM
= H( K+1, K ) +
H( K+2, K )*CONJG( VT( 2 ) ) +
H( K+3, K )*CONJG( VT( 3 ) )
.TP 19
.ti +4
H(
K+1, K ) = H( K+1, K ) -
CONJG( VT( 1 ) )*REFSUM
.TP 19
.ti +4
H(
K+2, K ) = ZERO
.TP 19
.ti +4
H(
K+3, K ) = ZERO
.TP 19
.ti +4
V(
1, M ) = VT( 1 )
.TP 19
.ti +4
V(
2, M ) = VT( 2 )
.TP 19
.ti +4
V(
3, M ) = VT( 3 )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
10
CONTINUE
.TP 19
.ti +4
K
= KRCOL + 3*( M22-1 )
.TP 19
.ti +4
IF(
BMP22 ) THEN
.TP 19
.ti +4
IF(
K.EQ.KTOP-1 ) THEN
.TP 19
.ti +4
CALL
CLAQR1( 2, H( K+1, K+1 ), LDH, S( 2*M22-1 ),
S( 2*M22 ), V( 1, M22 ) )
.TP 19
.ti +4
BETA
= V( 1, M22 )
.TP 19
.ti +4
CALL
CLARFG( 2, BETA, V( 2, M22 ), 1, V( 1, M22 ) )
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
BETA
= H( K+1, K )
.TP 19
.ti +4
V(
2, M22 ) = H( K+2, K )
.TP 19
.ti +4
CALL
CLARFG( 2, BETA, V( 2, M22 ), 1, V( 1, M22 ) )
.TP 19
.ti +4
H(
K+1, K ) = BETA
.TP 19
.ti +4
H(
K+2, K ) = ZERO
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
V(
1, M22 ) = ZERO
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
ACCUM ) THEN
.TP 19
.ti +4
JBOT
= MIN( NDCOL, KBOT )
.TP 19
.ti +4
ELSE
IF( WANTT ) THEN
.TP 19
.ti +4
JBOT
= N
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
JBOT
= KBOT
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
DO
30 J = MAX( KTOP, KRCOL ), JBOT
.TP 19
.ti +4
MEND
= MIN( MBOT, ( J-KRCOL+2 ) / 3 )
.TP 19
.ti +4
DO
20 M = MTOP, MEND
.TP 19
.ti +4
K
= KRCOL + 3*( M-1 )
.TP 19
.ti +4
REFSUM
= CONJG( V( 1, M ) )*
( H( K+1, J )+CONJG( V( 2, M ) )*H( K+2, J )+
CONJG( V( 3, M ) )*H( K+3, J ) )
.TP 19
.ti +4
H(
K+1, J ) = H( K+1, J ) - REFSUM
.TP 19
.ti +4
H(
K+2, J ) = H( K+2, J ) - REFSUM*V( 2, M )
.TP 19
.ti +4
H(
K+3, J ) = H( K+3, J ) - REFSUM*V( 3, M )
.TP 19
.ti +4
20
CONTINUE
.TP 19
.ti +4
30
CONTINUE
.TP 19
.ti +4
IF(
BMP22 ) THEN
.TP 19
.ti +4
K
= KRCOL + 3*( M22-1 )
.TP 19
.ti +4
DO
40 J = MAX( K+1, KTOP ), JBOT
.TP 19
.ti +4
REFSUM
= CONJG( V( 1, M22 ) )*
( H( K+1, J )+CONJG( V( 2, M22 ) )*
H( K+2, J ) )
.TP 19
.ti +4
H(
K+1, J ) = H( K+1, J ) - REFSUM
.TP 19
.ti +4
H(
K+2, J ) = H( K+2, J ) - REFSUM*V( 2, M22 )
.TP 19
.ti +4
40
CONTINUE
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
ACCUM ) THEN
.TP 19
.ti +4
JTOP
= MAX( KTOP, INCOL )
.TP 19
.ti +4
ELSE
IF( WANTT ) THEN
.TP 19
.ti +4
JTOP
= 1
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
JTOP
= KTOP
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
DO
80 M = MTOP, MBOT
.TP 19
.ti +4
IF(
V( 1, M ).NE.ZERO ) THEN
.TP 19
.ti +4
K
= KRCOL + 3*( M-1 )
.TP 19
.ti +4
DO
50 J = JTOP, MIN( KBOT, K+3 )
.TP 19
.ti +4
REFSUM
= V( 1, M )*( H( J, K+1 )+V( 2, M )*
H( J, K+2 )+V( 3, M )*H( J, K+3 ) )
.TP 19
.ti +4
H(
J, K+1 ) = H( J, K+1 ) - REFSUM
.TP 19
.ti +4
H(
J, K+2 ) = H( J, K+2 ) -
REFSUM*CONJG( V( 2, M ) )
.TP 19
.ti +4
H(
J, K+3 ) = H( J, K+3 ) -
REFSUM*CONJG( V( 3, M ) )
.TP 19
.ti +4
50
CONTINUE
.TP 19
.ti +4
IF(
ACCUM ) THEN
.TP 19
.ti +4
KMS
= K - INCOL
.TP 19
.ti +4
DO
60 J = MAX( 1, KTOP-INCOL ), KDU
.TP 19
.ti +4
REFSUM
= V( 1, M )*( U( J, KMS+1 )+V( 2, M )*
U( J, KMS+2 )+V( 3, M )*U( J, KMS+3 ) )
.TP 19
.ti +4
U(
J, KMS+1 ) = U( J, KMS+1 ) - REFSUM
.TP 19
.ti +4
U(
J, KMS+2 ) = U( J, KMS+2 ) -
REFSUM*CONJG( V( 2, M ) )
.TP 19
.ti +4
U(
J, KMS+3 ) = U( J, KMS+3 ) -
REFSUM*CONJG( V( 3, M ) )
.TP 19
.ti +4
60
CONTINUE
.TP 19
.ti +4
ELSE
IF( WANTZ ) THEN
.TP 19
.ti +4
DO
70 J = ILOZ, IHIZ
.TP 19
.ti +4
REFSUM
= V( 1, M )*( Z( J, K+1 )+V( 2, M )*
Z( J, K+2 )+V( 3, M )*Z( J, K+3 ) )
.TP 19
.ti +4
Z(
J, K+1 ) = Z( J, K+1 ) - REFSUM
.TP 19
.ti +4
Z(
J, K+2 ) = Z( J, K+2 ) -
REFSUM*CONJG( V( 2, M ) )
.TP 19
.ti +4
Z(
J, K+3 ) = Z( J, K+3 ) -
REFSUM*CONJG( V( 3, M ) )
.TP 19
.ti +4
70
CONTINUE
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
80
CONTINUE
.TP 19
.ti +4
K
= KRCOL + 3*( M22-1 )
.TP 19
.ti +4
IF(
BMP22 .AND. ( V( 1, M22 ).NE.ZERO ) ) THEN
.TP 19
.ti +4
DO
90 J = JTOP, MIN( KBOT, K+3 )
.TP 19
.ti +4
REFSUM
= V( 1, M22 )*( H( J, K+1 )+V( 2, M22 )*
H( J, K+2 ) )
.TP 19
.ti +4
H(
J, K+1 ) = H( J, K+1 ) - REFSUM
.TP 19
.ti +4
H(
J, K+2 ) = H( J, K+2 ) -
REFSUM*CONJG( V( 2, M22 ) )
.TP 19
.ti +4
90
CONTINUE
.TP 19
.ti +4
IF(
ACCUM ) THEN
.TP 19
.ti +4
KMS
= K - INCOL
.TP 19
.ti +4
DO
100 J = MAX( 1, KTOP-INCOL ), KDU
.TP 19
.ti +4
REFSUM
= V( 1, M22 )*( U( J, KMS+1 )+V( 2, M22 )*
U( J, KMS+2 ) )
.TP 19
.ti +4
U(
J, KMS+1 ) = U( J, KMS+1 ) - REFSUM
.TP 19
.ti +4
U(
J, KMS+2 ) = U( J, KMS+2 ) -
REFSUM*CONJG( V( 2, M22 ) )
.TP 19
.ti +4
100
CONTINUE
.TP 19
.ti +4
ELSE
IF( WANTZ ) THEN
.TP 19
.ti +4
DO
110 J = ILOZ, IHIZ
.TP 19
.ti +4
REFSUM
= V( 1, M22 )*( Z( J, K+1 )+V( 2, M22 )*
Z( J, K+2 ) )
.TP 19
.ti +4
Z(
J, K+1 ) = Z( J, K+1 ) - REFSUM
.TP 19
.ti +4
Z(
J, K+2 ) = Z( J, K+2 ) -
REFSUM*CONJG( V( 2, M22 ) )
.TP 19
.ti +4
110
CONTINUE
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
MSTART
= MTOP
.TP 19
.ti +4
IF(
KRCOL+3*( MSTART-1 ).LT.KTOP )
MSTART = MSTART + 1
.TP 19
.ti +4
MEND
= MBOT
.TP 19
.ti +4
IF(
BMP22 )
MEND = MEND + 1
.TP 19
.ti +4
IF(
KRCOL.EQ.KBOT-2 )
MEND = MEND + 1
.TP 19
.ti +4
DO
120 M = MSTART, MEND
.TP 19
.ti +4
K
= MIN( KBOT-1, KRCOL+3*( M-1 ) )
.TP 19
.ti +4
IF(
H( K+1, K ).NE.ZERO ) THEN
.TP 19
.ti +4
TST1
= CABS1( H( K, K ) ) + CABS1( H( K+1, K+1 ) )
.TP 19
.ti +4
IF(
TST1.EQ.RZERO ) THEN
.TP 19
.ti +4
IF(
K.GE.KTOP+1 )
TST1 = TST1 + CABS1( H( K, K-1 ) )
.TP 19
.ti +4
IF(
K.GE.KTOP+2 )
TST1 = TST1 + CABS1( H( K, K-2 ) )
.TP 19
.ti +4
IF(
K.GE.KTOP+3 )
TST1 = TST1 + CABS1( H( K, K-3 ) )
.TP 19
.ti +4
IF(
K.LE.KBOT-2 )
TST1 = TST1 + CABS1( H( K+2, K+1 ) )
.TP 19
.ti +4
IF(
K.LE.KBOT-3 )
TST1 = TST1 + CABS1( H( K+3, K+1 ) )
.TP 19
.ti +4
IF(
K.LE.KBOT-4 )
TST1 = TST1 + CABS1( H( K+4, K+1 ) )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
CABS1( H( K+1, K ) ).LE.MAX( SMLNUM, ULP*TST1 ) )
THEN
.TP 19
.ti +4
H12
= MAX( CABS1( H( K+1, K ) ),
CABS1( H( K, K+1 ) ) )
.TP 19
.ti +4
H21
= MIN( CABS1( H( K+1, K ) ),
CABS1( H( K, K+1 ) ) )
.TP 19
.ti +4
H11
= MAX( CABS1( H( K+1, K+1 ) ),
CABS1( H( K, K )-H( K+1, K+1 ) ) )
.TP 19
.ti +4
H22
= MIN( CABS1( H( K+1, K+1 ) ),
CABS1( H( K, K )-H( K+1, K+1 ) ) )
.TP 19
.ti +4
SCL
= H11 + H12
.TP 19
.ti +4
TST2
= H22*( H11 / SCL )
.TP 19
.ti +4
IF(
TST2.EQ.RZERO .OR. H21*( H12 / SCL ).LE.
MAX( SMLNUM, ULP*TST2 ) )H( K+1, K ) = ZERO
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
120
CONTINUE
.TP 19
.ti +4
MEND
= MIN( NBMPS, ( KBOT-KRCOL-1 ) / 3 )
.TP 19
.ti +4
DO
130 M = MTOP, MEND
.TP 19
.ti +4
K
= KRCOL + 3*( M-1 )
.TP 19
.ti +4
REFSUM
= V( 1, M )*V( 3, M )*H( K+4, K+3 )
.TP 19
.ti +4
H(
K+4, K+1 ) = -REFSUM
.TP 19
.ti +4
H(
K+4, K+2 ) = -REFSUM*CONJG( V( 2, M ) )
.TP 19
.ti +4
H(
K+4, K+3 ) = H( K+4, K+3 ) - REFSUM*CONJG( V( 3, M ) )
.TP 19
.ti +4
130
CONTINUE
.TP 19
.ti +4
140
CONTINUE
.TP 19
.ti +4
IF(
ACCUM ) THEN
.TP 19
.ti +4
IF(
WANTT ) THEN
.TP 19
.ti +4
JTOP
= 1
.TP 19
.ti +4
JBOT
= N
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
JTOP
= KTOP
.TP 19
.ti +4
JBOT
= KBOT
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
( .NOT.BLK22 ) .OR. ( INCOL.LT.KTOP ) .OR.
( NDCOL.GT.KBOT ) .OR. ( NS.LE.2 ) ) THEN
.TP 19
.ti +4
K1
= MAX( 1, KTOP-INCOL )
.TP 19
.ti +4
NU
= ( KDU-MAX( 0, NDCOL-KBOT ) ) - K1 + 1
.TP 19
.ti +4
DO
150 JCOL = MIN( NDCOL, KBOT ) + 1, JBOT, NH
.TP 19
.ti +4
JLEN
= MIN( NH, JBOT-JCOL+1 )
.TP 19
.ti +4
CALL
CGEMM( \(aqC\(aq, \(aqN\(aq, NU, JLEN, NU, ONE, U( K1, K1 ),
LDU, H( INCOL+K1, JCOL ), LDH, ZERO, WH,
LDWH )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, NU, JLEN, WH, LDWH,
H( INCOL+K1, JCOL ), LDH )
.TP 19
.ti +4
150
CONTINUE
.TP 19
.ti +4
DO
160 JROW = JTOP, MAX( KTOP, INCOL ) - 1, NV
.TP 19
.ti +4
JLEN
= MIN( NV, MAX( KTOP, INCOL )-JROW )
.TP 19
.ti +4
CALL
CGEMM( \(aqN\(aq, \(aqN\(aq, JLEN, NU, NU, ONE,
H( JROW, INCOL+K1 ), LDH, U( K1, K1 ),
LDU, ZERO, WV, LDWV )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, JLEN, NU, WV, LDWV,
H( JROW, INCOL+K1 ), LDH )
.TP 19
.ti +4
160
CONTINUE
.TP 19
.ti +4
IF(
WANTZ ) THEN
.TP 19
.ti +4
DO
170 JROW = ILOZ, IHIZ, NV
.TP 19
.ti +4
JLEN
= MIN( NV, IHIZ-JROW+1 )
.TP 19
.ti +4
CALL
CGEMM( \(aqN\(aq, \(aqN\(aq, JLEN, NU, NU, ONE,
Z( JROW, INCOL+K1 ), LDZ, U( K1, K1 ),
LDU, ZERO, WV, LDWV )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, JLEN, NU, WV, LDWV,
Z( JROW, INCOL+K1 ), LDZ )
.TP 19
.ti +4
170
CONTINUE
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
I2
= ( KDU+1 ) / 2
.TP 19
.ti +4
I4
= KDU
.TP 19
.ti +4
J2
= I4 - I2
.TP 19
.ti +4
J4
= KDU
.TP 19
.ti +4
KZS
= ( J4-J2 ) - ( NS+1 )
.TP 19
.ti +4
KNZ
= NS + 1
.TP 19
.ti +4
DO
180 JCOL = MIN( NDCOL, KBOT ) + 1, JBOT, NH
.TP 19
.ti +4
JLEN
= MIN( NH, JBOT-JCOL+1 )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, KNZ, JLEN, H( INCOL+1+J2, JCOL ),
LDH, WH( KZS+1, 1 ), LDWH )
.TP 19
.ti +4
CALL
CLASET( \(aqALL\(aq, KZS, JLEN, ZERO, ZERO, WH, LDWH )
.TP 19
.ti +4
CALL
CTRMM( \(aqL\(aq, \(aqU\(aq, \(aqC\(aq, \(aqN\(aq, KNZ, JLEN, ONE,
U( J2+1, 1+KZS ), LDU, WH( KZS+1, 1 ),
LDWH )
.TP 19
.ti +4
CALL
CGEMM( \(aqC\(aq, \(aqN\(aq, I2, JLEN, J2, ONE, U, LDU,
H( INCOL+1, JCOL ), LDH, ONE, WH, LDWH )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, J2, JLEN, H( INCOL+1, JCOL ), LDH,
WH( I2+1, 1 ), LDWH )
.TP 19
.ti +4
CALL
CTRMM( \(aqL\(aq, \(aqL\(aq, \(aqC\(aq, \(aqN\(aq, J2, JLEN, ONE,
U( 1, I2+1 ), LDU, WH( I2+1, 1 ), LDWH )
.TP 19
.ti +4
CALL
CGEMM( \(aqC\(aq, \(aqN\(aq, I4-I2, JLEN, J4-J2, ONE,
U( J2+1, I2+1 ), LDU,
H( INCOL+1+J2, JCOL ), LDH, ONE,
WH( I2+1, 1 ), LDWH )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, KDU, JLEN, WH, LDWH,
H( INCOL+1, JCOL ), LDH )
.TP 19
.ti +4
180
CONTINUE
.TP 19
.ti +4
DO
190 JROW = JTOP, MAX( INCOL, KTOP ) - 1, NV
.TP 19
.ti +4
JLEN
= MIN( NV, MAX( INCOL, KTOP )-JROW )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, JLEN, KNZ, H( JROW, INCOL+1+J2 ),
LDH, WV( 1, 1+KZS ), LDWV )
.TP 19
.ti +4
CALL
CLASET( \(aqALL\(aq, JLEN, KZS, ZERO, ZERO, WV, LDWV )
.TP 19
.ti +4
CALL
CTRMM( \(aqR\(aq, \(aqU\(aq, \(aqN\(aq, \(aqN\(aq, JLEN, KNZ, ONE,
U( J2+1, 1+KZS ), LDU, WV( 1, 1+KZS ),
LDWV )
.TP 19
.ti +4
CALL
CGEMM( \(aqN\(aq, \(aqN\(aq, JLEN, I2, J2, ONE,
H( JROW, INCOL+1 ), LDH, U, LDU, ONE, WV,
LDWV )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, JLEN, J2, H( JROW, INCOL+1 ), LDH,
WV( 1, 1+I2 ), LDWV )
.TP 19
.ti +4
CALL
CTRMM( \(aqR\(aq, \(aqL\(aq, \(aqN\(aq, \(aqN\(aq, JLEN, I4-I2, ONE,
U( 1, I2+1 ), LDU, WV( 1, 1+I2 ), LDWV )
.TP 19
.ti +4
CALL
CGEMM( \(aqN\(aq, \(aqN\(aq, JLEN, I4-I2, J4-J2, ONE,
H( JROW, INCOL+1+J2 ), LDH,
U( J2+1, I2+1 ), LDU, ONE, WV( 1, 1+I2 ),
LDWV )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, JLEN, KDU, WV, LDWV,
H( JROW, INCOL+1 ), LDH )
.TP 19
.ti +4
190
CONTINUE
.TP 19
.ti +4
IF(
WANTZ ) THEN
.TP 19
.ti +4
DO
200 JROW = ILOZ, IHIZ, NV
.TP 19
.ti +4
JLEN
= MIN( NV, IHIZ-JROW+1 )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, JLEN, KNZ,
Z( JROW, INCOL+1+J2 ), LDZ,
WV( 1, 1+KZS ), LDWV )
.TP 19
.ti +4
CALL
CLASET( \(aqALL\(aq, JLEN, KZS, ZERO, ZERO, WV,
LDWV )
.TP 19
.ti +4
CALL
CTRMM( \(aqR\(aq, \(aqU\(aq, \(aqN\(aq, \(aqN\(aq, JLEN, KNZ, ONE,
U( J2+1, 1+KZS ), LDU, WV( 1, 1+KZS ),
LDWV )
.TP 19
.ti +4
CALL
CGEMM( \(aqN\(aq, \(aqN\(aq, JLEN, I2, J2, ONE,
Z( JROW, INCOL+1 ), LDZ, U, LDU, ONE,
WV, LDWV )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, JLEN, J2, Z( JROW, INCOL+1 ),
LDZ, WV( 1, 1+I2 ), LDWV )
.TP 19
.ti +4
CALL
CTRMM( \(aqR\(aq, \(aqL\(aq, \(aqN\(aq, \(aqN\(aq, JLEN, I4-I2, ONE,
U( 1, I2+1 ), LDU, WV( 1, 1+I2 ),
LDWV )
.TP 19
.ti +4
CALL
CGEMM( \(aqN\(aq, \(aqN\(aq, JLEN, I4-I2, J4-J2, ONE,
Z( JROW, INCOL+1+J2 ), LDZ,
U( J2+1, I2+1 ), LDU, ONE,
WV( 1, 1+I2 ), LDWV )
.TP 19
.ti +4
CALL
CLACPY( \(aqALL\(aq, JLEN, KDU, WV, LDWV,
Z( JROW, INCOL+1 ), LDZ )
.TP 19
.ti +4
200
CONTINUE
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
210
CONTINUE
.TP 19
.ti +4
END
.SH PURPOSE
