function Y = bsp02(X)
% Test function for minimization,
% Wood's function
% cf. Himmelbau, p. 454 in FORTRAN program
Y =  (X(1) + 10*X(2))^2 +   5*(X(3) - X(4))^2 ...
   + (X(2) -  2*X(3))^4 + 110*(X(1) - X(4))^4;
