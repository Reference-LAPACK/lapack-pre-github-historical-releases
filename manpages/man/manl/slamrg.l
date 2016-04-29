.TH  SLAMRG 1 "November 2006" " LAPACK routine (version 3.1) " " LAPACK routine (version 3.1) " 
.SH NAME
SLAMRG - create a permutation list which will merge the elements of A (which is composed of two independently sorted sets) into a single set which is sorted in ascending order
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAMRG(
N1, N2, A, STRD1, STRD2, INDEX )
.TP 19
.ti +4
INTEGER
N1, N2, STRD1, STRD2
.TP 19
.ti +4
INTEGER
INDEX( * )
.TP 19
.ti +4
REAL
A( * )
.SH PURPOSE
SLAMRG will create a permutation list which will merge the elements
of A (which is composed of two independently sorted sets) into a
single set which is sorted in ascending order.

.SH ARGUMENTS
.TP 7
N1     (input) INTEGER
N2     (input) INTEGER
These arguements contain the respective lengths of the two
sorted lists to be merged.
.TP 7
A      (input) REAL array, dimension (N1+N2)
The first N1 elements of A contain a list of numbers which
are sorted in either ascending or descending order.  Likewise
for the final N2 elements.
.TP 7
STRD1  (input) INTEGER
STRD2  (input) INTEGER
These are the strides to be taken through the array A.
Allowable strides are 1 and -1.  They indicate whether a
subset of A is sorted in ascending (STRDx = 1) or descending
(STRDx = -1) order.
.TP 7
INDEX  (output) INTEGER array, dimension (N1+N2)
On exit this array will contain a permutation such that
if B( I ) = A( INDEX( I ) ) for I=1,N1+N2, then B will be
sorted in ascending order.
