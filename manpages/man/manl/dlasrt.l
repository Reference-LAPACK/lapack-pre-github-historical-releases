.TH  DLASRT 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
DLASRT - number in D in increasing order (if ID = \(aqI\(aq) or in decreasing order (if ID = \(aqD\(aq )
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASRT(
ID, N, D, INFO )
.TP 19
.ti +4
CHARACTER
ID
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
DOUBLE
PRECISION D( * )
.SH PURPOSE
Sort the numbers in D in increasing order (if ID = \(aqI\(aq) or
in decreasing order (if ID = \(aqD\(aq ).

Use Quick Sort, reverting to Insertion sort on arrays of
.br
size <= 20. Dimension of STACK limits N to about 2**32.
.br

.SH ARGUMENTS
.TP 8
ID      (input) CHARACTER*1
= \(aqI\(aq: sort D in increasing order;
.br
= \(aqD\(aq: sort D in decreasing order.
.TP 8
N       (input) INTEGER
The length of the array D.
.TP 8
D       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the array to be sorted.
On exit, D has been sorted into increasing order
(D(1) <= ... <= D(N) ) or into decreasing order
(D(1) >= ... >= D(N) ), depending on ID.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
