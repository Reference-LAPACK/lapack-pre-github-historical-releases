.TH  SLAGV2 1 "November 2006" " LAPACK auxiliary routine (version 3.1) " " LAPACK auxiliary routine (version 3.1) " 
.SH NAME
SLAGV2 - the Generalized Schur factorization of a real 2-by-2 matrix pencil (A,B) where B is upper triangular
.SH SYNOPSIS
.TP 19
SUBROUTINE SLAGV2(
A, LDA, B, LDB, ALPHAR, ALPHAI, BETA, CSL, SNL,
CSR, SNR )
.TP 19
.ti +4
INTEGER
LDA, LDB
.TP 19
.ti +4
REAL
CSL, CSR, SNL, SNR
.TP 19
.ti +4
REAL
A( LDA, * ), ALPHAI( 2 ), ALPHAR( 2 ),
B( LDB, * ), BETA( 2 )
.SH PURPOSE
SLAGV2 computes the Generalized Schur factorization of a real 2-by-2
matrix pencil (A,B) where B is upper triangular. This routine
computes orthogonal (rotation) matrices given by CSL, SNL and CSR,
SNR such that
.br

1) if the pencil (A,B) has two real eigenvalues (include 0/0 or 1/0
   types), then
.br

   [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
   [  0  a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]

   [ b11 b12 ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
   [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ],

2) if the pencil (A,B) has a pair of complex conjugate eigenvalues,
   then
.br

   [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
   [ a21 a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]

   [ b11  0  ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
   [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ]

   where b11 >= b22 > 0.
.br


.SH ARGUMENTS
.TP 8
A       (input/output) REAL array, dimension (LDA, 2)
On entry, the 2 x 2 matrix A.
On exit, A is overwritten by the ``A-part\(aq\(aq of the
generalized Schur form.
.TP 8
LDA     (input) INTEGER
THe leading dimension of the array A.  LDA >= 2.
.TP 8
B       (input/output) REAL array, dimension (LDB, 2)
On entry, the upper triangular 2 x 2 matrix B.
On exit, B is overwritten by the ``B-part\(aq\(aq of the
generalized Schur form.
.TP 8
LDB     (input) INTEGER
THe leading dimension of the array B.  LDB >= 2.
.TP 8
ALPHAR  (output) REAL array, dimension (2)
ALPHAI  (output) REAL array, dimension (2)
BETA    (output) REAL array, dimension (2)
(ALPHAR(k)+i*ALPHAI(k))/BETA(k) are the eigenvalues of the
pencil (A,B), k=1,2, i = sqrt(-1).  Note that BETA(k) may
be zero.
.TP 8
CSL     (output) REAL
The cosine of the left rotation matrix.
.TP 8
SNL     (output) REAL
The sine of the left rotation matrix.
.TP 8
CSR     (output) REAL
The cosine of the right rotation matrix.
.TP 8
SNR     (output) REAL
The sine of the right rotation matrix.
.SH FURTHER DETAILS
Based on contributions by
.br
   Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA

