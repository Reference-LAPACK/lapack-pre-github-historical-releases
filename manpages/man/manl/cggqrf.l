.SH NAME
CGGQRF - a generalized QR factorization of an N-by-M matrix A and an N-by-P matrix B
.SH SYNOPSIS
.TP 19
SUBROUTINE CGGQRF(
N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK,
LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, M, N, P
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), TAUA( * ), TAUB( * ),
WORK( * )
.SH PURPOSE
CGGQRF computes a generalized QR factorization of an N-by-M matrix A
and an N-by-P matrix B:

            A = Q*R,        B = Q*T*Z,
.br

where Q is an N-by-N unitary matrix, Z is a P-by-P unitary matrix,
and R and T assume one of the forms:
.br

if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
                (  0  ) N-M                         N   M-N
                   M
.br

where R11 is upper triangular, and
.br

if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
                 P-N  N                           ( T21 ) P
                                                     P
.br

where T12 or T21 is upper triangular.
.br

In particular, if B is square and nonsingular, the GQR factorization
of A and B implicitly gives the QR factorization of inv(B)*A:

             inv(B)*A = Z\(aq*(inv(T)*R)
.br

where inv(B) denotes the inverse of the matrix B, and Z\(aq denotes the
conjugate transpose of matrix Z.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of rows of the matrices A and B. N >= 0.
.TP 8
M       (input) INTEGER
The number of columns of the matrix A.  M >= 0.
.TP 8
P       (input) INTEGER
The number of columns of the matrix B.  P >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,M)
On entry, the N-by-M matrix A.
On exit, the elements on and above the diagonal of the array
contain the min(N,M)-by-M upper trapezoidal matrix R (R is
upper triangular if N >= M); the elements below the diagonal,
with the array TAUA, represent the unitary matrix Q as a
product of min(N,M) elementary reflectors (see Further
Details).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,N).
.TP 8
TAUA    (output) COMPLEX array, dimension (min(N,M))
The scalar factors of the elementary reflectors which
represent the unitary matrix Q (see Further Details).
B       (input/output) COMPLEX array, dimension (LDB,P)
On entry, the N-by-P matrix B.
On exit, if N <= P, the upper triangle of the subarray
B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
if N > P, the elements on and above the (N-P)-th subdiagonal
contain the N-by-P upper trapezoidal matrix T; the remaining
elements, with the array TAUB, represent the unitary
matrix Z as a product of elementary reflectors (see Further
Details).
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,N).
.TP 8
TAUB    (output) COMPLEX array, dimension (min(N,P))
The scalar factors of the elementary reflectors which
represent the unitary matrix Z (see Further Details).
WORK    (workspace/output) COMPLEX array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,N,M,P).
For optimum performance LWORK >= max(N,M,P)*max(NB1,NB2,NB3),
where NB1 is the optimal blocksize for the QR factorization
of an N-by-M matrix, NB2 is the optimal blocksize for the
RQ factorization of an N-by-P matrix, and NB3 is the optimal
blocksize for a call of CUNMQR.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(1) H(2) . . . H(k), where k = min(n,m).
.br

Each H(i) has the form
.br

   H(i) = I - taua * v * v\(aq
.br

where taua is a complex scalar, and v is a complex vector with
v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
and taua in TAUA(i).
.br
To form Q explicitly, use LAPACK subroutine CUNGQR.
.br
To use Q to update another matrix, use LAPACK subroutine CUNMQR.

The matrix Z is represented as a product of elementary reflectors

   Z = H(1) H(2) . . . H(k), where k = min(n,p).
.br

Each H(i) has the form
.br

   H(i) = I - taub * v * v\(aq
.br

where taub is a complex scalar, and v is a complex vector with
v(p-k+i+1:p) = 0 and v(p-k+i) = 1; v(1:p-k+i-1) is stored on exit in
B(n-k+i,1:p-k+i-1), and taub in TAUB(i).
.br
To form Z explicitly, use LAPACK subroutine CUNGRQ.
.br
To use Z to update another matrix, use LAPACK subroutine CUNMRQ.

