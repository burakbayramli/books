function x = fdnodes(gam,N,interval,tol)
%FDNODES Finite-difference nodes of varying density.
%	
%   x = fdnodes(gam,N)
%   x = fdnodes(gam,N,interval)
%   x = fdnodes(gam,N,interval,tol)
%	
%     gam : density parameter
%     N   : number of nodes, less 1
%     interval : scale nodes to cover this interval (default [-1,1])
%     tol : tolerance desired (use only if you get a warning)
%	
%   The returned x is an (N+1) column vector containing finite difference
%   nodes in [-1,1]. The distribution of nodes has density proportional to
%   (1-x^2)^(-gam). gam > 0 clusters nodes toward the endpoints, gam < 0
%   clusters toward the center, and gam = 0 is equispacing. gam = .5 gives
%   the Chebyshev extrema.
% 
%   Refer to sec 3.3 of _A Practical Guide to Pseudospectral Methods_, by B.
%   Fornberg.

% Copyright (c) 1996 by Toby Driscoll.
% slightly modified by Greg Fasshauer, Aug.1 2008, to always return column
% vector (N==1 case below)

if nargin < 4
  tol = 10*eps;
  if nargin < 3
    interval = [-1;1];
  end
end

if N==0
  x = 0;
  return
elseif N==1
  x = interval;
  sizex = size(x);
  if sizex(1) < sizex(2),   x = x.';  end    % always a column vector
  return
end

n = ceil(N/2)-1;			% # of nodes < 0 and > -1
c = gamma(1.5-gam)/(sqrt(pi)*gamma(1-gam)); % normalization
[gn,gw] = gaussj(24,0,-gam);

% Begin with equispaced
x = -1 + 2*(1:n)'/N;

fp = ones(n,1);
if gam
  f = ones(n,1);
else
  f = zeros(n,1);
end

% Newton iteration on integration of the density
iter = 0;
while (max(abs(f./fp)) > tol) & (iter < 40)
  GN = (gn+1)*(x.'+1)/2-1;
  GW = gw*((x.'+1)/2).^(1-gam);
  
  % f is discrepancy between integrated density and desired
  f = c*sum((1-GN).^(-gam).*GW).' - (1:n)'/N;
  % f' comes from Leibnitz's rule
  fp = c./(1-x.^2).^gam;
  x = x - f./fp;

  % x < -1 would cause complex numbers
  if gam > .8
    x = max(-1+4*eps,x);
  else
    x = max(-1+1e-5,x);
  end
  x = min(0,x);
  iter = iter+1;
end

if iter==40
  warning(sprintf('Max est error = %.2g\n',max(abs(f./fp))))
end

if rem(N,2)
  x = [-1;x;-flipud(x);1];
else
  x = [-1;x;0;-flipud(x);1];
end

resid = f;

x = (x+1)*diff(interval)/2 + interval(1);

