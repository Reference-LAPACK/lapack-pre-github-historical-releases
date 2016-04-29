.TH  SLAQR1 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME

.SH SYNOPSIS
.TP 19
SUBROUTINE SLAQR1(
N, H, LDH, SR1, SI1, SR2, SI2, V )
.TP 19
.ti +4
REAL
SI1, SI2, SR1, SR2
.TP 19
.ti +4
INTEGER
LDH, N
.TP 19
.ti +4
REAL
H( LDH, * ), V( * )
.TP 19
.ti +4
REAL
ZERO
.TP 19
.ti +4
PARAMETER
( ZERO = 0.0e0 )
.TP 19
.ti +4
REAL
H21S, H31S, S
.TP 19
.ti +4
INTRINSIC
ABS
.TP 19
.ti +4
IF(
N.EQ.2 ) THEN
.TP 19
.ti +4
S
= ABS( H( 1, 1 )-SR2 ) + ABS( SI2 ) + ABS( H( 2, 1 ) )
.TP 19
.ti +4
IF(
S.EQ.ZERO ) THEN
.TP 19
.ti +4
V(
1 ) = ZERO
.TP 19
.ti +4
V(
2 ) = ZERO
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
H21S
= H( 2, 1 ) / S
.TP 19
.ti +4
V(
1 ) = H21S*H( 1, 2 ) + ( H( 1, 1 )-SR1 )*
( ( H( 1, 1 )-SR2 ) / S ) - SI1*( SI2 / S )
.TP 19
.ti +4
V(
2 ) = H21S*( H( 1, 1 )+H( 2, 2 )-SR1-SR2 )
.TP 19
.ti +4
END
IF
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
S
= ABS( H( 1, 1 )-SR2 ) + ABS( SI2 ) + ABS( H( 2, 1 ) ) +
ABS( H( 3, 1 ) )
.TP 19
.ti +4
IF(
S.EQ.ZERO ) THEN
.TP 19
.ti +4
V(
1 ) = ZERO
.TP 19
.ti +4
V(
2 ) = ZERO
.TP 19
.ti +4
V(
3 ) = ZERO
.TP 19
.ti +4
ELSE
.TP 19
.ti +4
H21S
= H( 2, 1 ) / S
.TP 19
.ti +4
H31S
= H( 3, 1 ) / S
.TP 19
.ti +4
V(
1 ) = ( H( 1, 1 )-SR1 )*( ( H( 1, 1 )-SR2 ) / S ) -
SI1*( SI2 / S ) + H( 1, 2 )*H21S + H( 1, 3 )*H31S
.TP 19
.ti +4
V(
2 ) = H21S*( H( 1, 1 )+H( 2, 2 )-SR1-SR2 ) +
H( 2, 3 )*H31S
.TP 19
.ti +4
V(
3 ) = H31S*( H( 1, 1 )+H( 3, 3 )-SR1-SR2 ) +
H21S*H( 3, 2 )
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
.SH PURPOSE
