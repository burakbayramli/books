%GPS Toolbox
%Version 1.0 16-Oct-1997
%
%Directory: geodesy
%
%
%C2G      Convertion of cartesian coordinates (X,Y,Z) to geographical
%         coordinates (phi,lambda,h) on a selected reference ellipsoid
%
%C2GM     Convertion of cartesian coordinates (X,Y,Z) in m to geographical
%         coordinates (phi,lambda,h) in degrees, degrees and m on a
%         reference ellipsoid with semi-major axis a and flattening
%         f=1/invf
%
%         The iterative part is a modified implementation of
%         N. Bartelme and P. Meissl (1975)
%            Ein einfaches, rasches und numerisch stabiles
%            Verfahren zur Bestimmung des k\"urzesten Abstandes eines
%            Punktes von einem sph\"aroidischen Rotationsellipsoid,
%            Allgemeine Vermessungs-Nachrichten, Seite 436--439
%
%CORRDEMO Demonstration of the influence of correlation on
%         the mean value of two numbers
%
%DW       Script for investigation of the effect of change of weight on
%         the solution
%
%ELIMNOR  Eliminates the unknowns x1 from the normal equations
%                   [A1  A2] |x1| = |b1|
%                   [A2' A3] |x2|   |b2|
%         and solves for x2
%
%ELIMOBS  Eliminates the unknowns x1 from the observation equations
%                   [A1  A2] |x1| = b
%                            |x2|
%         and solves the weighted least squares problem for x2 from the
%         remaining observation equations
%
%ELLAXES  Computes the eigenvalues and -vectors of a 2 by 2 positive
%         definite matrix.
%
%ERRELL   usage:
%                      [xe,ye] = errell(xm,ym,cov)
%              or      [xe,ye] = errell(xm,ym,cov,nsig)
%         computes an error ellipse centered at(xm,ym) using
%         the covariance matrix cov associated with xm,ym.
%         If nsig is specified, it indicates the sigma level
%         (e.g. if nsig = 1 you get 1 sigma ellipse, if nsig = 2
%         you get the 2-sigma or ~86% conf. ellipse).
%         default value for nsig = 1. The vectors xe & ye
%         describe the ellipse centered about (xm,ym).
%
%FINDLOOP Nullspace of a matrix N = FINDLOOP(A) uses the pivoting LU
%         factorization computed by PLU and the resulting reduced row
%         echelon form computed by REF to find a matrix N whose columns
%         are a basis for the nullspace of A.  The number of columns
%         of N is the nullity of A.  If A has independent columns, then
%         N is empty.
%
%FINDNODE For a given n_value we find the component number iloc for
%         the node in the vector (of unknowns).
%
%FINDPIV  Used by PLU to find a pivot for Gaussian elimination.
%         [r,p] = FINDPIV(A(k:m,p:n),k,p,tol) finds the first element in
%         the specified submatrix which is larger than tol in absolute
%         value. It returns indices r and p so that A(r,p) is the pivot.
%
%FRGEOD   Subroutine to calculate Cartesian coordinates X,Y,Z
%         given geodetic coordinates latitude, longitude (east),
%         and height above reference ellipsoid along with
%         reference ellipsoid values semi-major axis (a)
%         and the inverse of flattening (finv)
%
%         The units of linear parameters h,a must agree (m,km,mi,..etc).
%         The input units of angular quantities must be in decimal degrees.
%         The output units of X,Y,Z will be the same as the units of h
%         and a.
%
%G2C      Convertion of geographical coordinates (phi,lambda,h) on a
%         selected reference ellipsoid to cartesian coordinates (X,Y,Z)
%
%GAUSS1   The first geodetical main problem solved iteratively
%         by means of Gauss' mid-latitude formulas.
%         Given the coordinates (phi1, lambda1) of a point and a
%         distance s and an azimuth az1 from here.
%         The coordinates (phi2,lambda2) and the reverse azimuth az2 are
%         unknowns.
%
%
%GAUSS2   The second geodetical main problem solved by means of
%         Gauss' mid-latitude formulas.
%         Given the coordinates (phi1, lambda1), and (phi2, lambda2)
%         of two points. The distance s and the mutual azimuths are
%         unknown.
%
%LEV      Least squares estimation of heights in a levelling
%         network as described by the following 3 data files:
%
%         levfix.dat, contains row-wise 2 columns
%                       point#   elevation
%
%         levfree.dat, contains row-wise 1 column
%                       point#
%
%         levobs.dat, contains row-wise 4 columns
%                       from-#  to-#     HDIFF   std. dev. of HDIFF
%
%LOCATE   For a given iprn_value we find the component
%         number iloc for the satellite in the vector
%         of unknowns
%
%LOOPLIST Each row in A contains from-node, to-node and observed value
%         along the oriented edge.  The vector v contains all loop sums
%         in an order corresponding to the rows in A.
%
%LOR      Linear orthogonal regression.
%         See Dahlquist & Bj›rck, 2nd edition, (1995) Example 7.6.2
%         The first column of Y contains y and the second column x
%         for p given points
%
%N_LOCATE Finds the index of node within an i_max by 1 vector of nodes
%
%NULL     Nullspace of a matrix
%         N = NULL(A) uses the pivoting LU factorization computed by
%         PLU and the resulting reduced row echelon form computed by REF to
%         find a matrix N whose columns are a basis for the nullspace of A.
%         The number of columns of N is the nullity of A.
%         If A has independent columns, then N is empty, N = [];
%
%         (This supersedes the MATLAB function NULL(A) which computes a
%         basis for the nullspace of A with orthonormal columns and, for
%         badly conditioned problems, even a possibly different dimension.)
%
%ONED     Code given by Mike Bevis, January 1997
%
%PLU      Pivoting, rectangular, LU factorization.  [P,L,U] = PLU(A),
%         for a rectangular matrix A, uses Gaussian elimination to
%         compute a permutation matrix P, a lower triangular matrix L
%         and an upper trapezoidal matrix U so that L*U = P*A.      U is the
%         same size as A. P and L are square, with as many rows as A.
%
%QR       Orthogonal-triangular decomposition.
%         [Q,R] = QR(A) produces an upper triangular matrix R of the same
%         dimension as A and a unitary matrix Q so that A = Q*R.
%
%         [Q,R,E] = QR(A) produces a permutation matrix E, an upper
%         triangular R and a unitary Q so that A*E = Q*R.  The column
%         permutation E is chosen so that abs(diag(R)) is decreasing.
%
%         [Q,R] = QR(A,0) produces the "economy size" decomposition.
%         If A is m-by-n with m > n, then only the first n columns of Q
%         are computed.
%
%         [Q,R,E] = QR(A,0) produces an "economy size" decomposition in
%         which E is a permutation vector, so that Q*R = A(:,E).  The
%         column permutation E is chosen so that abs(diag(R)) is
%         decreasing.
%
%         For sparse matrices, QR can compute a "Q-less QR decomposition",
%         which has the following slightly different behavior.
%
%         R = QR(A) returns only R.  Note that R = chol(A'*A).
%         [Q,R] = QR(A) returns both Q and R, but Q is often nearly full.
%         [C,R] = QR(A,B), where B has as many rows as A, returns C = Q'*B.
%         R = QR(A,0) and [C,R] = QR(A,B,0) produce economy size results.
%
%         The sparse version of QR does not do column permutations.
%         The full version of QR does not return C.
%
%         The least squares approximate solution to A*x = b can be found
%         with the Q-less QR decomposition and one step of iterative
%         refinement:
%                 x = R\(R'\(A'*b))
%                 r = b - A*x
%                 e = R\(R'\(A'*r))
%                 x = x + e;
%
%R2DMS    Conversion of radians to degrees, minutes, and seconds
%
%REF      Reduced Row Echelon Form.  R = ref(A) uses the pivoting LU
%         factorization computed by PLU to find the reduced row echelon
%         form of a rectangular matrix A.
%
%RELELLIP calculation of relative ellipse
%
%
%REPAIR   Repair of indefinite covariance matrix
%
%SETS     Station adjustment
%         The direction observations are written as a matrix. Each column
%         contains the observed values for the single direction in the
%         various rounds.
%         The reference direction is omitted.
%         Outputs are the adjusted directions, the orientation unknowns,
%         and the standard deviation of a direction observed with one
%         round.
%
%SIMIL    Similarity transformation between two sets of points
%         given by coordinates (x,y) and (xi,eta). The inputfile
%         contains the coordinates arranged in two columns:
%                            x1 y1
%                            x2 y2
%                              ...
%                            xp yp
%                            xi1 eta1
%                            xi2 eta2
%                              ...
%                           xip etap
%         Important: The order of points must be the same in both lists
%
%SUPPORT  Plot of support function and pertinent confidence
%         ellipse for a given 2 by 2 covariance matrix A
%
%TOGEOD   Subroutine to calculate geodetic coordinates
%         latitude, longitude, height given Cartesian
%         coordinates X,Y,Z, and reference ellipsoid
%         values semi-major axis (a) and the inverse
%         of flattening (finv).
%
%         The units of linear parameters X,Y,Z,a must all agree
%         (m,km,mi,ft,..etc). The output units of angular quantities
%         will be in decimal degrees (15.5 degrees not 15 deg 30 min).
%         The output units of h will be the same as the units of X,Y,Z,a.
%
%TWOC     Plot of 100 random positions and a confidence circle of radius c
%
%V_LOOPS  Each row in A contains from-node, to-node and three observed
%         coordinate differences along the oriented GPS-vector
%
%VECTORS  Ashtech Z-12 receiver observations made by a student group
%         attending a third year's course at Aalborg University, June 1994
%%%%%%%%%%%%%%%%%%%%% end contents.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
