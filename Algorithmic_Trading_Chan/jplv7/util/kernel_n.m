function [y, x] = kernel_n(u,ngrid,h)
% PURPOSE: normal kernel density estimate
% -------------------------------------------------------
% USAGE: [y x] = kernel_n(u,ngrid,h)
% where: u = vector or matrix of data
%    ngrid = number of grid points 
%   h = smoothing factor, h=0.01 is rough, h=large smooth
% -------------------------------------------------------
% RETURNS:
%           y = a vector or matrix of normal density estimates
%           x = the domain such that, plot(x,y) produces 
%               a plot of the density estimate
% -------------------------------------------------------
% SEE ALSO: pltdens()
% -------------------------------------------------------
% References              
% Hardle (1990), Smoothing Techniques, Springer-Verlag

if (nargin < 3),
error('Wrong number of arguments to kernel_n');
end;   
[n k] = size(u);
if (n < k); m = k; l = n; n = m; k = l; u = u'; end;
pf = zeros(ngrid,k); x = zeros(ngrid,k);
tt= 1:ngrid; c = 1/sqrt(2*pi)*ones(n,1);

for i=1:k; % loop over number of vectors k;
b1u = max(u(:,i)); b1l = min(u(:,i)); % determine the grid;
if (b1u > 0);
if (b1l < 0); grd = (b1u + abs(b1l))/ngrid; end;
if (b1l > 0); grd = (b1u - b1l)/ngrid; end;
end;
if (b1u < 0); grd = abs(b1u + b1l)/ngrid; end;
bs = b1l + tt*grd; 
 for j=1:ngrid;
 bv = bs(1,j); x(j,i) = bs(1,j); bvec = bv*ones(n,1);
 term1 = ((bvec-u(:,i)).*(bvec-u(:,i)))*(1/h);
 tmp = c.*exp(-0.5*term1);
 pf(j,i) = sum(tmp.*(1/(h*n)));
 end; 
end;
% convert to probability density
y = zeros(ngrid,k);
for i=1:k;
psum = sum(pf(:,i));
y(:,i) = pf(:,i)./psum;
end;

