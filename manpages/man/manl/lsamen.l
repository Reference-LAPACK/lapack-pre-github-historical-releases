.TH  LSAMEN 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
LSAMEN - if the first N letters of CA are the same as the first N letters of CB, regardless of case
.SH SYNOPSIS
.TP 17
LOGICAL FUNCTION
LSAMEN( N, CA, CB )
.TP 17
.ti +4
CHARACTER*(
* ) CA, CB
.TP 17
.ti +4
INTEGER
N
.SH PURPOSE
LSAMEN  tests if the first N letters of CA are the same as the
first N letters of CB, regardless of case.
LSAMEN returns .TRUE. if CA and CB are equivalent except for case
and .FALSE. otherwise.  LSAMEN also returns .FALSE. if LEN( CA )
or LEN( CB ) is less than N.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of characters in CA and CB to be compared.
.TP 8
CA      (input) CHARACTER*(*)
CB      (input) CHARACTER*(*)
CA and CB specify two character strings of length at least N.
Only the first N characters of each string will be accessed.
