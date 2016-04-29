.TH  SLAQR2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME

.SH SYNOPSIS
.TP 19
SUBROUTINE SLAQR2(
WANTT, WANTZ, N, KTOP, KBOT, NW, H, LDH, ILOZ,
IHIZ, Z, LDZ, NS, ND, SR, SI, V, LDV, NH, T,
LDT, NV, WV, LDWV, WORK, LWORK )
.TP 19
.ti +4
INTEGER
IHIZ, ILOZ, KBOT, KTOP, LDH, LDT, LDV, LDWV,
LDZ, LWORK, N, ND, NH, NS, NV, NW
.TP 19
.ti +4
LOGICAL
WANTT, WANTZ
.TP 19
.ti +4
REAL
H( LDH, * ), SI( * ), SR( * ), T( LDT, * ),
V( LDV, * ), WORK( * ), WV( LDWV, * ),
Z( LDZ, * )
.TP 19
.ti +4
REAL
ZERO, ONE
.TP 19
.ti +4
PARAMETER
( ZERO = 0.0e0, ONE = 1.0e0 )
.TP 19
.ti +4
REAL
AA, BB, BETA, CC, CS, DD, EVI, EVK, FOO, S,
SAFMAX, SAFMIN, SMLNUM, SN, TAU, ULP
.TP 19
.ti +4
INTEGER
I, IFST, ILST, INFO, INFQR, J, JW, K, KCOL,
KEND, KLN, KROW, KWTOP, LTOP, LWK1, LWK2,
LWKOPT
.TP 19
.ti +4
LOGICAL
BULGE, SORTED
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
EXTERNAL
SCOPY, SGEHRD, SGEMM, SLABAD, SLACPY, SLAHQR,
SLANV2, SLARF, SLARFG, SLASET, SORGHR, STREXC
.TP 19
.ti +4
INTRINSIC
ABS, INT, MAX, MIN, REAL, SQRT
.TP 19
.ti +4
JW
= MIN( NW, KBOT-KTOP+1 )
.TP 19
.ti +4
IF(
JW.LE.2 ) THEN
.TP 19
.ti +4
LWKOPT
= 1
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
CALL
SGEHRD( JW, 1, JW-1, T, LDT, WORK, WORK, -1, INFO )
.TP 19
.ti +4
LWK1
= INT( WORK( 1 ) )
.TP 19
.ti +4
CALL
SORGHR( JW, 1, JW-1, T, LDT, WORK, WORK, -1, INFO )
.TP 19
.ti +4
LWK2
= INT( WORK( 1 ) )
.TP 19
.ti +4
LWKOPT
= JW + MAX( LWK1, LWK2 )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
LWORK.EQ.-1 ) THEN
.TP 19
.ti +4
WORK(
1 ) = REAL( LWKOPT )
.TP 19
.ti +4
RETURN
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
NS
= 0
.TP 19
.ti +4
ND
= 0
.TP 19
.ti +4
IF(
KTOP.GT.KBOT )
RETURN
.TP 19
.ti +4
IF(
NW.LT.1 )
RETURN
.TP 19
.ti +4
SAFMIN
= SLAMCH( \(aqSAFE MINIMUM\(aq )
.TP 19
.ti +4
SAFMAX
= ONE / SAFMIN
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
JW
= MIN( NW, KBOT-KTOP+1 )
.TP 19
.ti +4
KWTOP
= KBOT - JW + 1
.TP 19
.ti +4
IF(
KWTOP.EQ.KTOP ) THEN
.TP 19
.ti +4
S
= ZERO
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
S
= H( KWTOP, KWTOP-1 )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
KBOT.EQ.KWTOP ) THEN
.TP 19
.ti +4
SR(
KWTOP ) = H( KWTOP, KWTOP )
.TP 19
.ti +4
SI(
KWTOP ) = ZERO
.TP 19
.ti +4
NS
= 1
.TP 19
.ti +4
ND
= 0
.TP 19
.ti +4
IF(
ABS( S ).LE.MAX( SMLNUM, ULP*ABS( H( KWTOP, KWTOP ) ) ) )
THEN
.TP 19
.ti +4
NS
= 0
.TP 19
.ti +4
ND
= 1
.TP 19
.ti +4
IF(
KWTOP.GT.KTOP )
H( KWTOP, KWTOP-1 ) = ZERO
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
RETURN
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
CALL
SLACPY( \(aqU\(aq, JW, JW, H( KWTOP, KWTOP ), LDH, T, LDT )
.TP 19
.ti +4
CALL
SCOPY( JW-1, H( KWTOP+1, KWTOP ), LDH+1, T( 2, 1 ), LDT+1 )
.TP 19
.ti +4
CALL
SLASET( \(aqA\(aq, JW, JW, ZERO, ONE, V, LDV )
.TP 19
.ti +4
CALL
SLAHQR( .true., .true., JW, 1, JW, T, LDT, SR( KWTOP ),
SI( KWTOP ), 1, JW, V, LDV, INFQR )
.TP 19
.ti +4
DO
10 J = 1, JW - 3
.TP 19
.ti +4
T(
J+2, J ) = ZERO
.TP 19
.ti +4
T(
J+3, J ) = ZERO
.TP 19
.ti +4
10
CONTINUE
.TP 19
.ti +4
IF(
JW.GT.2 )
T( JW, JW-2 ) = ZERO
.TP 19
.ti +4
NS
= JW
.TP 19
.ti +4
ILST
= INFQR + 1
.TP 19
.ti +4
20
CONTINUE
.TP 19
.ti +4
IF(
ILST.LE.NS ) THEN
.TP 19
.ti +4
IF(
NS.EQ.1 ) THEN
.TP 19
.ti +4
BULGE
= .FALSE.
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
BULGE
= T( NS, NS-1 ).NE.ZERO
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
.NOT.BULGE ) THEN
.TP 19
.ti +4
FOO
= ABS( T( NS, NS ) )
.TP 19
.ti +4
IF(
FOO.EQ.ZERO )
FOO = ABS( S )
.TP 19
.ti +4
IF(
ABS( S*V( 1, NS ) ).LE.MAX( SMLNUM, ULP*FOO ) ) THEN
.TP 19
.ti +4
NS
= NS - 1
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
IFST
= NS
.TP 19
.ti +4
CALL
STREXC( \(aqV\(aq, JW, T, LDT, V, LDV, IFST, ILST, WORK,
INFO )
.TP 19
.ti +4
ILST
= ILST + 1
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
FOO
= ABS( T( NS, NS ) ) + SQRT( ABS( T( NS, NS-1 ) ) )*
SQRT( ABS( T( NS-1, NS ) ) )
.TP 19
.ti +4
IF(
FOO.EQ.ZERO )
FOO = ABS( S )
.TP 19
.ti +4
IF(
MAX( ABS( S*V( 1, NS ) ), ABS( S*V( 1, NS-1 ) ) ).LE.
MAX( SMLNUM, ULP*FOO ) ) THEN
.TP 19
.ti +4
NS
= NS - 2
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
IFST
= NS
.TP 19
.ti +4
CALL
STREXC( \(aqV\(aq, JW, T, LDT, V, LDV, IFST, ILST, WORK,
INFO )
.TP 19
.ti +4
ILST
= ILST + 2
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
GO
TO 20
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
NS.EQ.0 )
S = ZERO
.TP 19
.ti +4
IF(
NS.LT.JW ) THEN
.TP 19
.ti +4
SORTED
= .false.
.TP 19
.ti +4
I
= NS + 1
.TP 19
.ti +4
30
CONTINUE
.TP 19
.ti +4
IF(
SORTED )
GO TO 50
.TP 19
.ti +4
SORTED
= .true.
.TP 19
.ti +4
KEND
= I - 1
.TP 19
.ti +4
I
= INFQR + 1
.TP 19
.ti +4
IF(
I.EQ.NS ) THEN
.TP 19
.ti +4
K
= I + 1
.TP 19
.ti +4
ELSE
IF( T( I+1, I ).EQ.ZERO ) THEN
.TP 19
.ti +4
K
= I + 1
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
K
= I + 2
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
40
CONTINUE
.TP 19
.ti +4
IF(
K.LE.KEND ) THEN
.TP 19
.ti +4
IF(
K.EQ.I+1 ) THEN
.TP 19
.ti +4
EVI
= ABS( T( I, I ) )
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
EVI
= ABS( T( I, I ) ) + SQRT( ABS( T( I+1, I ) ) )*
SQRT( ABS( T( I, I+1 ) ) )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
K.EQ.KEND ) THEN
.TP 19
.ti +4
EVK
= ABS( T( K, K ) )
.TP 19
.ti +4
ELSE
IF( T( K+1, K ).EQ.ZERO ) THEN
.TP 19
.ti +4
EVK
= ABS( T( K, K ) )
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
EVK
= ABS( T( K, K ) ) + SQRT( ABS( T( K+1, K ) ) )*
SQRT( ABS( T( K, K+1 ) ) )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
EVI.GE.EVK ) THEN
.TP 19
.ti +4
I
= K
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
SORTED
= .false.
.TP 19
.ti +4
IFST
= I
.TP 19
.ti +4
ILST
= K
.TP 19
.ti +4
CALL
STREXC( \(aqV\(aq, JW, T, LDT, V, LDV, IFST, ILST, WORK,
INFO )
.TP 19
.ti +4
IF(
INFO.EQ.0 ) THEN
.TP 19
.ti +4
I
= ILST
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
I
= K
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
IF(
I.EQ.KEND ) THEN
.TP 19
.ti +4
K
= I + 1
.TP 19
.ti +4
ELSE
IF( T( I+1, I ).EQ.ZERO ) THEN
.TP 19
.ti +4
K
= I + 1
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
K
= I + 2
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
GO
TO 40
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
GO
TO 30
.TP 19
.ti +4
50
CONTINUE
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
I
= JW
.TP 19
.ti +4
60
CONTINUE
.TP 19
.ti +4
IF(
I.GE.INFQR+1 ) THEN
.TP 19
.ti +4
IF(
I.EQ.INFQR+1 ) THEN
.TP 19
.ti +4
SR(
KWTOP+I-1 ) = T( I, I )
.TP 19
.ti +4
SI(
KWTOP+I-1 ) = ZERO
.TP 19
.ti +4
I
= I - 1
.TP 19
.ti +4
ELSE
IF( T( I, I-1 ).EQ.ZERO ) THEN
.TP 19
.ti +4
SR(
KWTOP+I-1 ) = T( I, I )
.TP 19
.ti +4
SI(
KWTOP+I-1 ) = ZERO
.TP 19
.ti +4
I
= I - 1
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
AA
= T( I-1, I-1 )
.TP 19
.ti +4
CC
= T( I, I-1 )
.TP 19
.ti +4
BB
= T( I-1, I )
.TP 19
.ti +4
DD
= T( I, I )
.TP 19
.ti +4
CALL
SLANV2( AA, BB, CC, DD, SR( KWTOP+I-2 ),
SI( KWTOP+I-2 ), SR( KWTOP+I-1 ),
SI( KWTOP+I-1 ), CS, SN )
.TP 19
.ti +4
I
= I - 2
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
GO
TO 60
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
NS.LT.JW .OR. S.EQ.ZERO ) THEN
.TP 19
.ti +4
IF(
NS.GT.1 .AND. S.NE.ZERO ) THEN
.TP 19
.ti +4
CALL
SCOPY( NS, V, LDV, WORK, 1 )
.TP 19
.ti +4
BETA
= WORK( 1 )
.TP 19
.ti +4
CALL
SLARFG( NS, BETA, WORK( 2 ), 1, TAU )
.TP 19
.ti +4
WORK(
1 ) = ONE
.TP 19
.ti +4
CALL
SLASET( \(aqL\(aq, JW-2, JW-2, ZERO, ZERO, T( 3, 1 ), LDT )
.TP 19
.ti +4
CALL
SLARF( \(aqL\(aq, NS, JW, WORK, 1, TAU, T, LDT,
WORK( JW+1 ) )
.TP 19
.ti +4
CALL
SLARF( \(aqR\(aq, NS, NS, WORK, 1, TAU, T, LDT,
WORK( JW+1 ) )
.TP 19
.ti +4
CALL
SLARF( \(aqR\(aq, JW, NS, WORK, 1, TAU, V, LDV,
WORK( JW+1 ) )
.TP 19
.ti +4
CALL
SGEHRD( JW, 1, NS, T, LDT, WORK, WORK( JW+1 ),
LWORK-JW, INFO )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
KWTOP.GT.1 )
H( KWTOP, KWTOP-1 ) = S*V( 1, 1 )
.TP 19
.ti +4
CALL
SLACPY( \(aqU\(aq, JW, JW, T, LDT, H( KWTOP, KWTOP ), LDH )
.TP 19
.ti +4
CALL
SCOPY( JW-1, T( 2, 1 ), LDT+1, H( KWTOP+1, KWTOP ),
LDH+1 )
.TP 19
.ti +4
IF(
NS.GT.1 .AND. S.NE.ZERO ) THEN
.TP 19
.ti +4
CALL
SORGHR( JW, 1, NS, T, LDT, WORK, WORK( JW+1 ),
LWORK-JW, INFO )
.TP 19
.ti +4
CALL
SGEMM( \(aqN\(aq, \(aqN\(aq, JW, NS, NS, ONE, V, LDV, T, LDT, ZERO,
WV, LDWV )
.TP 19
.ti +4
CALL
SLACPY( \(aqA\(aq, JW, NS, WV, LDWV, V, LDV )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
WANTT ) THEN
.TP 19
.ti +4
LTOP
= 1
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
LTOP
= KTOP
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
DO
70 KROW = LTOP, KWTOP - 1, NV
.TP 19
.ti +4
KLN
= MIN( NV, KWTOP-KROW )
.TP 19
.ti +4
CALL
SGEMM( \(aqN\(aq, \(aqN\(aq, KLN, JW, JW, ONE, H( KROW, KWTOP ),
LDH, V, LDV, ZERO, WV, LDWV )
.TP 19
.ti +4
CALL
SLACPY( \(aqA\(aq, KLN, JW, WV, LDWV, H( KROW, KWTOP ), LDH )
.TP 19
.ti +4
70
CONTINUE
.TP 19
.ti +4
IF(
WANTT ) THEN
.TP 19
.ti +4
DO
80 KCOL = KBOT + 1, N, NH
.TP 19
.ti +4
KLN
= MIN( NH, N-KCOL+1 )
.TP 19
.ti +4
CALL
SGEMM( \(aqC\(aq, \(aqN\(aq, JW, KLN, JW, ONE, V, LDV,
H( KWTOP, KCOL ), LDH, ZERO, T, LDT )
.TP 19
.ti +4
CALL
SLACPY( \(aqA\(aq, JW, KLN, T, LDT, H( KWTOP, KCOL ),
LDH )
.TP 19
.ti +4
80
CONTINUE
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
IF(
WANTZ ) THEN
.TP 19
.ti +4
DO
90 KROW = ILOZ, IHIZ, NV
.TP 19
.ti +4
KLN
= MIN( NV, IHIZ-KROW+1 )
.TP 19
.ti +4
CALL
SGEMM( \(aqN\(aq, \(aqN\(aq, KLN, JW, JW, ONE, Z( KROW, KWTOP ),
LDZ, V, LDV, ZERO, WV, LDWV )
.TP 19
.ti +4
CALL
SLACPY( \(aqA\(aq, KLN, JW, WV, LDWV, Z( KROW, KWTOP ),
LDZ )
.TP 19
.ti +4
90
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
ND
= JW - NS
.TP 19
.ti +4
NS
= NS - INFQR
.TP 19
.ti +4
WORK(
1 ) = REAL( LWKOPT )
.TP 19
.ti +4
END
.SH PURPOSE
