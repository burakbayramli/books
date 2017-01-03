function y = crlag(x,n)
% PURPOSE: circular lag function
% -----------------------------------------------------
% USAGE: y = crlag(x,n)
% where  x = input vector tx1
%        n = # of values to return
% -----------------------------------------------------
% RETURNS: y = x(n)
%              x(2)
%               .
%              x(n-1)
% -----------------------------------------------------
             
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
% Modified for speed by
% Kevin Sheppard
% kksheppard@ucsd.edu

y= zeros(n,1);
y(1)=x(n);
y(2:n)=x(1:n-1);


