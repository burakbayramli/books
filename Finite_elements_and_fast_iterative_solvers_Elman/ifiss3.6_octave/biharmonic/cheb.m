function [D,x] = cheb(N)
%CHEB   from Trefethen Approximation Theory book p.54
%   [D,x] = cheb(N);
%   input
%          N          number of collocation points
%   output
%          D          1-d differentiation matrix
%          x          1-d Chebyshev grid
if N==0, D=0; x=1; return, end
x = cos(pi*(0:N)/N)';
c = [2; ones(N-1,1); 2] .* (-1).^(0:N)';
X = repmat(x,1,N+1);
dX = X-X';
D = (c*(1./c)')./(dX + (eye(N+1)));
D = D - diag(sum(D'));
return
