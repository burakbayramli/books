function rnd = unif_rnd (n,a,b)
% PURPOSE: returns a uniform random number between a,b 
%---------------------------------------------------
% USAGE: cdf = unif_rnd(n,a,b)
% where: a = scalar left limit
%        b = scalar right limit
%        n = number of draws (default = 1)
% NOTE: mean = (a+b)/2, variance = (b-a)^2/12
%---------------------------------------------------
% RETURNS: rnd scalar or vector
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin < 2
    error('Wrong # of arguments to unif_rnd');
elseif nargin < 3
    b = a;
    a = n;
    n = 1;   
end

rnd = a + (b - a) .* rand(n, 1);
