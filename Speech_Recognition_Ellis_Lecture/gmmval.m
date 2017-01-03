function r = gmmval(x,m,v,c)
% v = gmmval(x,m,v,c)  Evaluate a gaussian mixture model at points
%    x is a set of data points, one per row.  m, v and c define 
%    a Gaussian mixture model's means, covariances and mix priors 
%    respectively.  v returns the full GMM evaluated at each x.
% 2001-02-11 dpwe@ee.columbia.edu
% $Header: $

[nmix, ndim] = size(m);
ndat = size(x,1);

r = zeros(ndat,1);

pistuff = (2*pi) ^ -(ndim/2);

% Calculate posterior data memberships for each component
for k = 1:nmix
  % Reconstruct covar mx
  if nmix > 1 | size(v,1) == 1
    cv = reshape(v(k,:),ndim,ndim);
  else
    cv = v;
  end
  mu = ones(ndat,1)*m(k,:);
  xmm = x - mu;
  % Evaluate Gaussians
  if ndim == 1
    % Matlab syntax bites us
    px = pistuff*exp(-0.5*   (xmm'.*(inv(cv)*xmm')))/sqrt(det(cv));
  else
    px = pistuff*exp(-0.5*sum(xmm'.*(inv(cv)*xmm')))/sqrt(det(cv));
  end
  r = r + c(k)*px';
end

