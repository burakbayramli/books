function y = pois_rnd(n,lambda)
% PURPOSE: generate random draws from the possion distribution
%--------------------------------------------------
% USAGE:  y = pois_rnd(n,lambda)
% where:   lambda = mean used for the draws
%               n = # of draws
%--------------------------------------------------  
% RETURNS: y = a vector of draws
%--------------------------------------------------  
% NOTES:  Uses naive inversion method.
% mean = lambda, variance = lambda
%--------------------------------------------------
                    
% Gordon K Smyth
% gks@maths.uq.edu.au
% Department of Mathematics, Unversity of Queensland, Q 4072, Australia
% http://www.maths.uq.edu.au/~gks/

% modified by J.P. LeSage 1/98

if nargin ~= 2
error('Wrong # of arguments to pois_rnd');
end;

low = max( 0 , floor( lambda - 8*sqrt(lambda) ));
hi = ceil( lambda + 8*sqrt(lambda) + 4/lambda );
x = (low:hi)';
p = cumsum( exp( -lambda + x.*log(lambda) - gammaln(x+1) ));
u = rand(n,1);
y = zeros(n,1);
for i = 1:n,
   k = find( u(i) < p );
   y(i) = x( k(1) );
end;
