function yi = linterp(x,y,xi)
% linterp    Piecewise linear interpolation in a table of (x,y) data
%
% Synposis:  yi = linterp(x,y,xi)
%
% Input:     x,y = vectors containing the tabulated data
%            xi  = value of x at which function y = f(x) is desired
%
% Output:    yi = value of y at xi obtained by linear interpolation

i = binSearch(x,xi);                %  Find appropriate data pair
L1 = (x(i+1) - xi)/(x(i+1) - x(i)); %  Evaluate basis functions
L2 = (xi - x(i))/(x(i+1) - x(i));
yi = y(i)*L1 + y(i+1)*L2;           %  Evaluate interpolant
