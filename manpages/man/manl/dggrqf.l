.SH NAME
DGGRQF - a generalized RQ factorization of an M-by-N matrix A and a P-by-N matrix B
.SH SYNOPSIS
.TP 19
SUBROUTINE DGGRQF(
M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK,
LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, M, N, P
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), TAUA( * ), TAUB( * ),
WORK( * )
.SH PURPOSE
DGGRQF computes a generalized RQ factorization of an M-by-N matrix A
and a P-by-N matrix B:

            A = R*Q,        B = Z*T*Q,
.br

where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
matrix, and R and T assume one of the forms:
.br

if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
                 N-M  M                           ( R21 ) N
                                                     N
.br

where R12 or R21 is upper triangular, and
.br

if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
                (  0  ) P-N                         P   N-P
                   N
.br

where T11 is upper triangular.
.br

In particular, if B is square and nonsingular, the GRQ factorization
of A and B implicitly gives the RQ factorization of A*inv(B):

             A*inv(B) = (R*inv(T))*Z\(aq
.br

where inv(B) denotes the inverse of the matrix B, and Z\(aq denotes the
transpose of the matrix Z.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
P       (input) INTEGER
The number of rows of the matrix B.  P >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrices A and B. N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, if M <= N, the upper triangle of the subarray
A(1:M,N-M+1:N) contains the M-by-M upper triangular matrix R;
if M > N, the elements on and above the (M-N)-th subdiagonal
contain the M-by-N upper trapezoidal matrix R; the remaining
elements, with the array TAUA, represent the orthogonal
matrix Q as a product of elementary reflectors (see Further
Details).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
TAUA    (output) DOUBLE PRECISION array, dimension (min(M,N))
The scalar factors of the elementary reflectors which
represent the orthogonal matrix Q (see Further Details).
B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
On entry, the P-by-N matrix B.
On exit, the elements on and above the diagonal of the array
contain the min(P,N)-by-N upper trapezoidal matrix T (T is
upper triangular if P >= N); the elements below the diagonal,
with the array TAUB, represent the orthogonal matrix Z as a
product of elementary reflectors (see Further Details).
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,P).
.TP 8
TAUB    (output) DOUBLE PRECISION array, dimension (min(P,N))
The scalar factors of the elementary reflectors which
represent the orthogonal matrix Z (see Further Details).
WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,N,M,P).
For optimum performance LWORK >= max(N,M,P)*max(NB1,NB2,NB3),
where NB1 is the optimal blocksize for the RQ factorization
of an M-by-N matrix, NB2 is the optimal blocksize for the
QR factorization of a P-by-N matrix, and NB3 is the optimal
blocksize for a call of DORMRQ.

If LWORK = -1, then a workspace query is assumed; the routine
only calculates the optimal size of the WORK array, returns
this value as the first entry of the WORK array, and no error
message related to LWORK is issued by XERBLA.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INF0= -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(1) H(2) . . . H(k), where k = min(m,n).
.br

Each H(i) has the form
.br

   H(i) = I - taua * v * v\(aq
.br

where taua is a real scalar, and v is a real vector with
.br
v(n-k+i+1:n) = 0 and v(n-k+i) = 1; v(1:n-k+i-1) is stored on exit in
A(m-k+i,1:n-k+i-1), and taua in TAUA(i).
.br
To form Q explicitly, use LAPACK subroutine DORGRQ.
.br
To use Q to update another matrix, use LAPACK subroutine DORMRQ.

The matrix Z is represented as a product of elementary reflectors

   Z = H(1) H(2) . . . H(k), where k = min(p,n).
.br

Each H(i) has the form
.br

   H(i) = I - taub * v * v\(aq
.br

where taub is a real scalar, and v is a real vector with
.br
v(1:i-1) = 0 and v(i) = 1; v(i+1:p) is stored on exit in B(i+1:p,i),
and taub in TAUB(i).
.br
To form Z explicitly, use LAPACK subroutine DORGQR.
.br
To use Z to update another matrix, use LAPACK subroutine DORMQR.

