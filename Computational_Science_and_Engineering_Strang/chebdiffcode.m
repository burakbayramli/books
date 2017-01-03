%5.4  chebdiffcode.m

function [DC,x] = chebdiff(N)      % DC = differentiation matrix on Chebyshev grid
x = cos(pi*(0:N)/N)'; c = [2; ones(N-1,1); 2].*(-1).^(0:N)';
X = repmat(x,1,N+1); dX = X - X';
DC = (c*(1./c)')./(dX+(eye(N+1))); % off-diagonal entries of DC
DC = DC - diag(sum(DC'));          % rows of DC = DC_N add to zero
