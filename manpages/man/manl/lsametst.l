.TH  LSAMETST 1 "November 2006" " LAPACK test routine (version 3.1) " " LAPACK test routine (version 3.1) " 
.SH NAME

.SH SYNOPSIS
.TP 14
PROGRAM TEST1
.TP 14
.ti +4
INTEGER
I1, I2
.TP 14
.ti +4
LOGICAL
LSAME
.TP 14
.ti +4
EXTERNAL
LSAME
.TP 14
.ti +4
INTRINSIC
ICHAR
.TP 14
.ti +4
I1
= ICHAR( \(aqA\(aq )
.TP 14
.ti +4
I2
= ICHAR( \(aqa\(aq )
.TP 14
.ti +4
IF(
I2-I1.EQ.32 ) THEN
.TP 14
.ti +4
WRITE(
*, * ) \(aq ASCII character set\(aq
.TP 14
.ti +4
ELSE
.TP 14
.ti +4
WRITE(
*, * ) \(aq Non-ASCII character set, IOFF should be \(aq,I2-I1
.TP 14
.ti +4
END
IF
.TP 14
.ti +4
IF(
.NOT.LSAME( \(aqA\(aq, \(aqA\(aq ) )
WRITE( *, 9999 )\(aqA\(aq, \(aqA\(aq
.TP 14
.ti +4
IF(
.NOT.LSAME( \(aqA\(aq, \(aqa\(aq ) )
WRITE( *, 9999 )\(aqA\(aq, \(aqa\(aq
.TP 14
.ti +4
IF(
.NOT.LSAME( \(aqa\(aq, \(aqA\(aq ) )
WRITE( *, 9999 )\(aqa\(aq, \(aqA\(aq
.TP 14
.ti +4
IF(
.NOT.LSAME( \(aqa\(aq, \(aqa\(aq ) )
WRITE( *, 9999 )\(aqa\(aq, \(aqa\(aq
.TP 14
.ti +4
IF(
LSAME( \(aqA\(aq, \(aqB\(aq ) )
WRITE( *, 9998 )\(aqA\(aq, \(aqB\(aq
.TP 14
.ti +4
IF(
LSAME( \(aqA\(aq, \(aqb\(aq ) )
WRITE( *, 9998 )\(aqA\(aq, \(aqb\(aq
.TP 14
.ti +4
IF(
LSAME( \(aqa\(aq, \(aqB\(aq ) )
WRITE( *, 9998 )\(aqa\(aq, \(aqB\(aq
.TP 14
.ti +4
IF(
LSAME( \(aqa\(aq, \(aqb\(aq ) )
WRITE( *, 9998 )\(aqa\(aq, \(aqb\(aq
.TP 14
.ti +4
IF(
LSAME( \(aqO\(aq, \(aq/\(aq ) )
WRITE( *, 9998 )\(aqO\(aq, \(aq/\(aq
.TP 14
.ti +4
IF(
LSAME( \(aq/\(aq, \(aqO\(aq ) )
WRITE( *, 9998 )\(aq/\(aq, \(aqO\(aq
.TP 14
.ti +4
IF(
LSAME( \(aqo\(aq, \(aq/\(aq ) )
WRITE( *, 9998 )\(aqo\(aq, \(aq/\(aq
.TP 14
.ti +4
IF(
LSAME( \(aq/\(aq, \(aqo\(aq ) )
WRITE( *, 9998 )\(aq/\(aq, \(aqo\(aq
.TP 14
.ti +4
WRITE(
*, * )\(aq Tests completed\(aq
.TP 14
.ti +4
9999
FORMAT( \(aq *** Error: LSAME( \(aq, A1, \(aq, \(aq, A1, \(aq) is .FALSE.\(aq )
.TP 14
.ti +4
9998
FORMAT( \(aq *** Error: LSAME( \(aq, A1, \(aq, \(aq, A1, \(aq) is .TRUE.\(aq )
.TP 14
.ti +4
END
.SH PURPOSE
