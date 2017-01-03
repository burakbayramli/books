function fjac = fdjac(f,x,varargin)
% PURPOSE: Computes two-sided finite difference Jacobian
% -------------------------------------------------------
% Usage: fjac = fdjac(func,x,varargin)
% Where: func = name of function of form fval = func(x)
%           x = vector of parameters (n x 1)
%    varargin = optional arguments passed to the function
% -------------------------------------------------------
% RETURNS:
%        fjac = finite differnce Jacobian
% -------------------------------------------------------
% See also: fdhess, hessian
% -------------------------------------------------------
        
% Code from:
% COMPECON toolbox [www4.ncsu.edu/~pfackler]
% documentation modified to fit the format of the Ecoometrics Toolbox
% by James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

eps = 1e-5;

h = eps^(1/3)*max(abs(x),1);
for j=1:length(x);
   x1 = x; x1(j) = x(j) + h(j);
   x0 = x; x0(j) = x(j) - h(j);
   fjac(:,j) = (feval(f,x1,varargin{:})-feval(f,x0,varargin{:}))/(x1(j)-x0(j));
end