.TH  SLASDT 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLASDT - a tree of subproblems for bidiagonal divide and conquer
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASDT(
N, LVL, ND, INODE, NDIML, NDIMR, MSUB )
.TP 19
.ti +4
INTEGER
LVL, MSUB, N, ND
.TP 19
.ti +4
INTEGER
INODE( * ), NDIML( * ), NDIMR( * )
.SH PURPOSE
SLASDT creates a tree of subproblems for bidiagonal divide and
conquer.

.SH ARGUMENTS
.TP 7
N      (input) INTEGER
On entry, the number of diagonal elements of the
bidiagonal matrix.
.TP 7
LVL    (output) INTEGER
On exit, the number of levels on the computation tree.
.TP 7
ND     (output) INTEGER
On exit, the number of nodes on the tree.
.TP 7
INODE  (output) INTEGER array, dimension ( N )
On exit, centers of subproblems.
.TP 7
NDIML  (output) INTEGER array, dimension ( N )
On exit, row dimensions of left children.
.TP 7
NDIMR  (output) INTEGER array, dimension ( N )
On exit, row dimensions of right children.
.TP 7
MSUB   (input) INTEGER.
On entry, the maximum row dimension each subproblem at the
bottom of the tree can be of.
.SH FURTHER DETAILS
Based on contributions by
.br
   Ming Gu and Huan Ren, Computer Science Division, University of
   California at Berkeley, USA
.br

