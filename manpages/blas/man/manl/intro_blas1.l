."
." An introductory man page for the BLAS1 subroutines.  Contains a brief discription of 
." each subroutine and many examples of how to correctly call the subroutines.
." This was written as a quick reference to determine the available BLAS routines and 
." how to call them.
." 
." To view this file use:
." $ groff -man -Tascii intro_blas1.man | less
."
." To install this man page for system wide access (as root):
." # cp intro_blas1.man /usr/local/man/man1/intro_blas1.1
." # chmod +r /usr/local/man/man1/intro_blas1.1
."
." comp.lang.fortran 
." sci.math.num-analysis
." lapack@cs.utk.edu
."
.TH INTRO_BLAS1 l "12 August 05"
.SH NAME
INTRO_BLAS1 - Introduction to vector-vector linear algebra (matrix) subprograms
.SH DESCRIPTION
The Level 1 BLAS perform basic vector-vector operations.  The
following three types of vector-vector operations are available:

Routines for scaling, copying, swapping, and computing linear
combination of vectors.

Routines for computing dot products between vectors and various vector
norms.

Routines for generating or applying plane or modified plane rotations.

The Basic Linear Algebra Subprograms (BLAS) were developed to enhance
the portability of published linear algebra codes.  Because these
subprograms are portable, modular, self-documenting, and efficient,
you can incorporate them into your programs.

To realize the full power of the BLAS you must understand the
following three subjects:

- FORTRAN storage of arrays

- FORTRAN array argument association

- BLAS indexing conventions
.SS FORTRAN storage of arrays
Arrays in FORTRAN are stored in column major order.  This means that
the eariler indexes in an array declaration toggle first.  Consider
the following specifications:

DIMENSION A(N1,N2),B(N3)
.RS 0
EQUIVALENCE (A,B)

where N3 = N1 * N2.  Then A(I,J) is associated with the same memory
location as B(K) where

K = I + (J-1) * N1

This means that successive elements of a column of A are adjacent in
memory, while successive elements of a row of A are stored with a
difference of N1 storage units between them.  Remember that the size
of a storage unit depends on the data type.
.SS FORTRAN array argument association
When a FORTRAN subprogram is called with an array element as argument,
the value is not passed.  Instead, the subprogram receives the address
in memory of the element.  Consider the following code segment:

.nf
M=11
N=13
REAL A(M,N)
COL = 3
CALL SUBR (A(1,COL),M)
 .
 .
 .
SUBROUTINE SUBR (X,N)
REAL X(N)
 .
 .
 .
.fi

In this example, the subroutine SUBR is given the address of the first
element of the third column of A.  Because it treats that argument as a
one-dimensional array, successive elements X(1), X(2), ..., occupy the
same memory locations as successive elements of the \fIthird\fP column
of A: that is, A(1,3), A(2,3), ....  Hence, the entire third column of
A is available to the subprogram.
.SS BLAS Indexing Conventions
The rest of this section describes the topics of manipulating array
sections, dealing with stride arguments, and handling backward storage.  

A vector description in BLAS is defined by three quantities:

- array or starting element within an array, for instance the variable
X or X(I,J)

- vector length or number of elements, for instance the variable N

- the increment, sometimes called the stride, that defines the number
of storage units \fIbetween\fP successive vector elements, for
instance the variable INCX.

The notation for describing a BLAS vector in calling a BLAS subroutine
is the triad (N,X,INCX).  A few very brief examples follow.  If X is a
one dimensional array of length N, then (N,X,1) represents forward
storage of X i.e. X(1), X(2), ..., X(N) and (N,X,-1) represents backward
storage of X i.e. X(N), X(N-1), ..., X(1).  If A is an M by N array,
then (M,A(1,J),1) represents column J and (N,A(I,1),M) represents row
I.  Finally, if an M by N matrix is embedded in the upper left-hand
corner of an array B of size LDB by NMAX, then column J is
(M,B(1,J),1) and row I is (N,B(I,1),LDB).  More specific details follow.
.SS Forward Storage

As an example of the BLAS vector declaration using the above, suppose
that X represents a declared real array.  Let N be the vector length
and let INCX be the increment.  Suppose that a logical vector x with
components x(i), i = 1, 2,..., N, is to be stored in X.  If INCX >= 0,
then x(i) is stored in X(1 + (I-1) * INCX).  This is known as forward
array storage starting at X(1) with stride equal to INCX, ending with
X(1 - (N-1) * INCX).  Thus, if N = 4 and INCX = 2, the logical vector
x with components x(1), x(2), x(3), and x(4) are stored in memory in
the array elements X(1), X(3), X(5), and X(7), respectively.

This method of indexing, using a starting element, a number of
elements, and a stride, is especially useful for accessing
one-dimensional vectors in multidimensional arrays.  For instance, if
A is defined as

REAL A(M,N)

Then to access the 2nd row of A, one uses forward storage with an
stride of M.  Thus a BLAS routine call with 

X=A(2,1)

and increment/stride of 

INCX=M

will access A(2,i) for i = 1,2,...,N.  To access the third column of A
in a BLAS routine call with

X=A(1,3)

and increment/stride of 

INCX=1

This approach also works with multidimensional arrays.  As an example,
if A is defined as

REAL A(M,N,P)

to access the P elements of A at row 3 and column 4 one could call a
BLAS routine with starting address X of 

X=A(3,4,1)

and increment/stride of

INCX=M*N
.SS Backward Storage

Some BLAS routines permit backward storage of vectors, which is
specified by using a negative increment INCX.  If INCX < 0, then x(i)
is stored "backwards" in X.  Specifically x(i) is stored in X(1 +
(N-I) * |INCX|) or equivalently in X(1 - (N-I) * INCX).  This is
called backward storage starting from X(1 - (N-1) * INCX) with stride
equal to INCX, ending with X(1).  Thus, if N = 4 and INCX = -2, the
logical vector components x(1), x(2), x(3), and x(4) are stored in the
array elements X(7), X(5), X(3), and X(1), respectively.

Note: INCX = 0 is permitted by some BLAS routines and is not permitted
by others.  When it is allowed, it means that logical vector x is a
vector of length N, all whose components are equal the value of X(1).
.SS Further Stride Examples

The following examples illustrate how to use increment arguments to
perform different operations with the same subprogram.  These examples
use the BLAS function SDOT, with the following declarations:

.nf
INTEGER*4 N,INCX,INCY
REAL*4 SDOT,S,X(1+(N-1)*|INCX|),Y(1+(N-1)*|INCY|)
S = SDOT (N, X,INCX, Y,INCY)
.fi

This sets S to the dot product of the vectors (N,X,INCX) and (N,Y,INCY).

Example 1: Compute the dot product T = X(1)*Y(1) + X(2)*Y(2) + X(3)*Y(3) + X(4)*Y(4):

.nf
REAL*4 SDOT,T,X(4),Y(4)
T = SDOT (4, X,1, Y,1)
.fi

Example 2: Compute the convolution T = X(1)*Y(4) + X(2)*Y(3) + X(3)*Y(2) + X(4)*Y(1):

.nf
REAL*4 SDOT,T,X(4),Y(4)
T = SDOT (4, X,1, Y,-1)
.fi

Example 3: Compute the dot product Y(2) = A(2,1)*X(1) + A(2,2)*X(2) + A(2,3)*X(3), 
which is the dot product of the second row of an M by 3 matrix A, stored in a
10 by 3 array, with a 3-element vector X:

.nf
INTEGER*4 N,LDA
PARAMETER (LDA = 10)
REAL*4 SDOT,A(LDA,3),X(3),Y(LDA)
N = 3
Y(2) = SDOT (N, A(2,1),LDA, X,1)
.fi
.SS BLAS Data Types

The following data types are used in the BLAS routines:

- REAL: Fortran "real" data type, 32-bit floating point; these routine
names begin with S.

- COMPLEX: Fortran "complex" data type, two 32-bit floating point
reals; these routine names begin with C.

- DOUBLE PRECISION: Fortran "double precision" data type, 64-bit
floating point; these routine names begin with D.

- DOUBLE COMPLEX: Fortran "double complex" data type, two 64-bit
floating point doubles; these routine names begin with Z.
.SS BLAS Naming Conventions
The following table describes the naming conventions for these
routines:

.nf
-------------------------------------------------------------
                                                  64-bit
                                                  complex
                      64-bit real                 (double
                      (double       32-bit        complex
        32-bit real   precision)    complex       precision)
-------------------------------------------------------------	      
form:   Sname         Dname        Cname         Zname		      
example:SAXPY         DAXPY        CAXPY         ZAXPY		      
-------------------------------------------------------------
.fi
.SS FORTRAN type declaration for functions				     	  

Always declare the data type of external functions.  Declaring the
data type of the complex Level 1 BLAS functions is particularily
important because, based on the first letter of their names and the
Fortran data typing rules, the default implied data type would be
REAL.
     								     	  
Fortran type declarations for function names follow: 

.nf     								     	  
Type                  Function Name				     	  
     								     	  
REAL                  SASUM, SCASUM, SCNRM2, SDOT, SNRM2, SSUM	      
   								     	  
COMPLEX               CDOTC, CDOTU, CSUM				      
     								     	  
DOUBLE PRECISION      DASUM, DZASUM, DDOT, DNRM2, DZNRM2, DSUM	      
     								     	  
DOUBLE COMPLEX        ZDOTC, ZDOTU, ZSUM				      
     								     	  
INTEGER               ISAMAX, IDAMAX, ICAMAX, IZAMAX, ISAMIN, IDAMIN,    
                      ISMAX, IDMAX, ISMIN, IDMIN			      
.fi							     	  
.SS Summary Table of Level 1 BLAS Routines					     	  

The following table contains the purpose, operation, and name of each
Level 1 BLAS routine.  The first routine name listed in each table
block is the name of the manual page that contains documentation for
any routines listed in that block.  The routines marked with an
asterisk (*) are extensions to the standard set of Level 1 BLAS
routines.  For the complete details about each operation, see the
individual man pages.  Note: functions marked with an asterisk [*] are
extensions to the standard set of Level 1 BLAS routines that may not
be present on all systems.

The man(1) command can find a man page online by either the real,
complex, double precision, or double complex name.

.nf
--------------------------------------------------------------	      
Purpose                  Operation					      
--------------------------------------------------------------	  
Sums the absolute                          n             SASUM	      
values of the elements   sasum <- ||x|| = Sum |x |       DASUM	      
of a real vector (also                 1  i=1   i			      
called the l1 norm)						     	  
   								     	  
Sums the absolute        scasum <- ||Real[x]||  +        SCASUM	      
values of the real and                        1          DZASUM	      
imaginary parts of the            ||Imag[x]||  =			      
elements of a complex                        1			      
vector                             n				     	  
                                  Sum |Real[x ]| +			      
                                  i=1        i			      
   								     	  
                                   n				     	  
                                  Sum |Imag[x ]|			      
                                  i=1        i			      
   								     	  
Adds a scalar multiple   y <- alpha*x + beta*y           SAXPBY*	      
of a real or complex                                     DAXPBY*	      
vector to a scalar                                       CAXPBY*	      
multiple of another                                      ZAXPBY*	      
vector								      
   								     	  
Adds a scalar multiple   y <- alpha*x + y                SAXPY	      
of a real or complex                                     DAXPY	      
vector to another                                        CAXPY	      
vector                                                   ZAXPY	      
   								     	  
Copies a real or         y <- x                          SCOPY	      
complex vector into                                      DCOPY	      
another vector                                           CCOPY	      
                                                         ZCOPY	      
   								     	  
Computes a dot product            T       n              SDOT	      
of two real or complex   sdot <- x  y  = Sum x y         DDOT	      
vectors                                  i=1  i i			      
   								     	  
                                   H      n  _           CDOTC	      
                         cdotc <- x  y = Sum x y         ZDOTC	      
                                         i=1  i i			      
   								     	  
                                   T       n             CDOTU	      
                         cdotu <- x  y  = Sum x y        ZDOTU	      
                                          i=1  i i			      
   								     	  
Computes the Hadamard    z(i):=alpha x(i) y(i) + beta    SHAD*	      
product of two vectors   z(i)                            DHAD*	      
                                                         CHAD*	      
                                                         ZHAD*	      
   								     	  
Computes the Euclidean   snrm2 <- ||x|| =                SNRM2	      
norm (also called l2                   2                 DNRM2	      
norm) of a real or             n     2				      
complex vector           sqrt(Sum (x  )				      
                              i=1   i				      
   								     	  
                         scnrm2 <- ||x|| =               SCNRM2	      
                                        2                DZNRM2	      
                               n   _				     	  
                         sqrt(Sum (x  x )				      
                              i=1   i  i				      
   								     	  
   								     	  
Applies a real plane                                     CSROT*	      
rotation to a pair of                                    ZDROT*	      
complex vectors							      
   								     	  
Applies an orthogonal                                    SROT	      
plane rotation                                           DROT	      
   								     	  
Constructs a Givens                                      SROTG	      
plane rotation                                           DROTG	      
                                                         CROTG*	      
                                                         ZROTG*	      
   								     	  
Applies a modified                                       SROTM	      
Givens plane rotation                                    DROTM	      
   								     	  
Constructs a modified                                    SROTMG	      
Givens plane rotation                                    DROTMG	      
   								     	  
Scales a real or         x <- alpha x                    SSCAL	      
complex vector                                           DSCAL	      
                                                         CSCAL	      
                                                         ZSCAL	      
                                                         CSSCAL	      
                                                         ZDSCAL	      
   								     	  
Sums the elements of a           n                       SSUM*	      
real or complex vector   sum <- Sum x                    DSUM*	      
                                i=1  i                   CSUM*	      
                                                         ZSUM*	      
   								     	  
Swaps two real or two    x <-> y                         SSWAP	      
complex vectors                                          DSWAP	      
                                                         CSWAP	      
                                                         ZSWAP	      
   								     	  
Searches a vector for    isamax <- MAX |x |              ISAMAX	      
the first occurrence of                  j               IDAMAX	      
the maximum absolute                                     ICAMAX	      
value                                                    IZAMAX	      
   								     	  
Searches a vector for    isamin <- MIN |x |              ISAMIN*	      
the first occurrence of                  j               IDAMIN*	      
the minimum absolute						     	  
value								      
   								     	  
Searches a vector for    ismax <- MAX  x                 ISMAX*	      
the first occurrence of                 j                IDMAX*	      
the minimum absolute						     	  
value								      
   								     	  
Searches a vector for    ismin <- MIN x                  ISMIN*	      
the first occurrence of                j                 IDMIN*	      
the minimum absolute						     	  
value								      
--------------------------------------------------------------         
.fi
 
In addition to the mathematical functions defined above, several
search functions are a part of Level 1 BLAS; these functions are
listed below:

.nf					     	  
ISAMAX, ICAMAX, ISAMIN*, ISMAX*, ISMIN*			     	  
IDAMAX  IZAMAX, IDAMIN*, IDMAX*, IDMIN*
.fi
.SH TO DO

Many of the stared functions have not been implemented yet in a free software.
.SH SEE ALSO

intro_blas2(1), intro_blas3(1)
.SH REFERENCES

Lawson, C., Hanson, R., Kincaid, D., and Krogh, F., "Basic Linear
Algebra Subprograms for Fortran Usage," ACM Transactions on
Mathematical Software, 5 (1979), pp. 308 - 325.
.SH IMPLEMENTATION
See the individual man pages for implementation details and full 
argument listings
.SH AUTHOR
John L. Weatherwax
