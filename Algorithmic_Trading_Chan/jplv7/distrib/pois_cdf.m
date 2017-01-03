function f = pois_cdf(x,lambda)
% PURPOSE: computes the cumulative distribution function at x
% of the Poisson distribution with mean lambda
%---------------------------------------------------
% USAGE:     cdf = pois_cdf(x,lambda)
% where:     x   = variable vector (nx1)
%         lambda = mean 
%
%---------------------------------------------------
% Reference: Johnson & Kotz, Discrete Distributions, p 114

% Gordon K Smyth
% gks@maths.uq.edu.au
% Department of Mathematics, Unversity of Queensland, Q 4072, Australia
% http://www.maths.uq.edu.au/~gks/

% modified by J.P. LeSage 1/98

if nargin ~= 2
error('Wrong # of arguments to pois_cdf');
end;

if lambda <= 0
   error('Poisson mean must be positive.')
elseif x < 0
   f = 0;
else
   f = 1 - gammainc( floor(x+1), lambda );
end
