.SH NAME
ILAENV - called from the LAPACK routines to choose problem-dependent parameters for the local environment
.SH SYNOPSIS
.TP 17
INTEGER FUNCTION
ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
.TP 17
.ti +4
CHARACTER*(
* ) NAME, OPTS
.TP 17
.ti +4
INTEGER
ISPEC, N1, N2, N3, N4
.SH PURPOSE
ILAENV is called from the LAPACK routines to choose problem-dependent
parameters for the local environment.  See ISPEC for a description of
the parameters.
.br

ILAENV returns an INTEGER
.br
if ILAENV >= 0: ILAENV returns the value of the parameter specified by ISPEC
if ILAENV < 0:  if ILAENV = -k, the k-th argument had an illegal value.

This version provides a set of parameters which should give good,
but not optimal, performance on many of the currently available
computers.  Users are encouraged to modify this subroutine to set
the tuning parameters for their particular machine using the option
and problem size information in the arguments.
.br

This routine will not function correctly if it is converted to all
lower case.  Converting it to all upper case is allowed.
.br

.SH ARGUMENTS
.TP 8
ISPEC   (input) INTEGER
Specifies the parameter to be returned as the value of
ILAENV.
= 1: the optimal blocksize; if this value is 1, an unblocked
algorithm will give the best performance.
= 2: the minimum block size for which the block routine
should be used; if the usable block size is less than
this value, an unblocked routine should be used.
= 3: the crossover point (in a block routine, for N less
than this value, an unblocked routine should be used)
= 4: the number of shifts, used in the nonsymmetric
eigenvalue routines (DEPRECATED)
= 5: the minimum column dimension for blocking to be used;
rectangular blocks must have dimension at least k by m,
where k is given by ILAENV(2,...) and m by ILAENV(5,...)
= 6: the crossover point for the SVD (when reducing an m by n
matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
this value, a QR factorization is used first to reduce
the matrix to a triangular form.)
= 7: the number of processors
.br
= 8: the crossover point for the multishift QR method
for nonsymmetric eigenvalue problems (DEPRECATED)
= 9: maximum size of the subproblems at the bottom of the
computation tree in the divide-and-conquer algorithm
(used by xGELSD and xGESDD)
=10: ieee NaN arithmetic can be trusted not to trap
.br
=11: infinity arithmetic can be trusted not to trap
.br
12 <= ISPEC <= 16:
xHSEQR or one of its subroutines,
see IPARMQ for detailed explanation
.TP 8
NAME    (input) CHARACTER*(*)
The name of the calling subroutine, in either upper case or
lower case.
.TP 8
OPTS    (input) CHARACTER*(*)
The character options to the subroutine NAME, concatenated
into a single character string.  For example, UPLO = \(aqU\(aq,
TRANS = \(aqT\(aq, and DIAG = \(aqN\(aq for a triangular routine would
be specified as OPTS = \(aqUTN\(aq.
.TP 8
N1      (input) INTEGER
N2      (input) INTEGER
N3      (input) INTEGER
N4      (input) INTEGER
Problem dimensions for the subroutine NAME; these may not all
be required.
.SH FURTHER DETAILS
The following conventions have been used when calling ILAENV from the
LAPACK routines:
.br
1)  OPTS is a concatenation of all of the character options to
    subroutine NAME, in the same order that they appear in the
    argument list for NAME, even if they are not used in determining
    the value of the parameter specified by ISPEC.
.br
2)  The problem dimensions N1, N2, N3, N4 are specified in the order
    that they appear in the argument list for NAME.  N1 is used
    first, N2 second, and so on, and unused problem dimensions are
    passed a value of -1.
.br
3)  The parameter value returned by ILAENV is checked for validity in
    the calling subroutine.  For example, ILAENV is used to retrieve
    the optimal blocksize for STRTRI as follows:
.br

    NB = ILAENV( 1, \(aqSTRTRI\(aq, UPLO // DIAG, N, -1, -1, -1 )
    IF( NB.LE.1 ) NB = MAX( 1, N )
.br

