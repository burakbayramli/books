function out1 = skewtdis_rnd(nu,lambda,T,state)
% PURPOSE:
% returns random draws from Hansen's (1994) 'skewed t' distribution
% 
% USAGE:
% random = skewtdis_rnd(nu,lambda,T)
% 
% INPUTS:
% nu = a matrix or scalar degrees of freedom parameter 
% lammbda = a maxtrix or scalar skewness parameter 
% T = number of draws
% state, an integer to use to seed the random number generator
% 
% OUTPUTS:
% an Tx1 vector of draws from the dist'n  
% 
% COMMENTS:
% 
% Author: Andrew Patton
% Modified: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


if nargin<4
   rand('state',sum(1234*clock));	% setting RNG to new seed according to computer clock time.
else
   rand('state',state);
end

if size(nu,1)<T;
   nu = nu(1)*ones(T,1);
end
if size(lambda,1)<T;
   lambda = lambda(1)*ones(T,1);
end
c = gamma((nu+1)/2)./(sqrt(pi*(nu-2)).*gamma(nu/2));
a = 4*lambda.*c.*((nu-2)./(nu-1));
b = sqrt(1 + 3*lambda.^2 - a.^2);

rand('state',sum(100*clock));		% resetting state for random number generator
u = rand(T,1);
out1 = skewtdis_inv(u,nu,lambda);
