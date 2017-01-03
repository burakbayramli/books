function [mo,vo,co,jx] = gmmest(x,mi,vi,ci,its,viz)
% [mo,vo,co] = gmmest(x,mi,vi,ci,its,viz)   Estimate Gaussian mixture models
%     Model the distribution of data x with Gaussians.  
%     mi and vi specify initial means and variances for the models;
%       scalar <mi> just uses that many mixture components (default 5).
%     ci is initial mixture weights
%     its is the number of iterations to perform (default 20)
%     If viz is 1, plot first 2 dims at each iteration/ -1, just at end
%     mo is a matrix of means (one row per Gaussian) and 
%     vo is a matrix of (unravelled) covariance matrices.
%     co is the vector of mixture priors
%     jx is the 'fuzzy membership' (posterior mix weights) for each x
% 2001-02-11 dpwe@ee.columbia.edu
% $Header: $

if nargin < 2
  mi = 5;
end

% Global data properties
[ndat,ndim] = size(x);
meanx = mean(x);
covx = cov(x);
% Total number of points in cov (to unravel)
ncov = prod(size(covx));
% Eigen analysis of inverse covariance
[u,s,v] = svd(inv(covx));

if prod(size(mi)) == 1
  nmix = mi;
  % Random initial means around global mean, distributed like data
  mi = (ones(nmix,1) * meanx) + (v*inv(sqrt(s))*randn(ndim, nmix))';
else
  nmix=size(mi,1);
end

if nargin < 3
  vi = [];
end

if prod(size(vi)) == 0
  % Uniform covariances from global (each unraveled on a row)
  vi = ones(nmix,1)*covx(1:ncov);
end

if nargin < 4
  ci = [];
end

if prod(size(ci)) == 0
  ci = ones(1,nmix)/nmix;
end

if nargin < 5
  its = 20;
end

if nargin < 6
  viz = 0;
end

mo = mi;
vo = vi;
co = ci;

lik = -9999;

pistuff = (2*pi) ^ -(ndim/2);

jx = zeros(ndat, nmix);

if viz ~= 0 & ndim == 1
  % precompute histogram
  [histn, histx] = hist(x(:,1),100);
  vscale = max(histn);
end

for it = 1:its
  xlik = zeros(ndat,nmix);

  if viz == 1
    if ndim == 1
      bar(histx, histn);
      gmmplot(mo,vo,co,[1],vscale*min(sqrt(vo)./co'),'r');
    else
      plot(x(:,1),x(:,2),'.b');
      gmmplot(mo,vo,co,[1 2],1,'r');
    end    
    pause
  end
  
  % Calculate posterior data memberships for each component
  for c = 1:nmix
    % Reconstruct covar mx
    cv = reshape(vo(c,:),ndim,ndim);
    mu = ones(ndat,1)*mo(c,:);
    xmm = x - mu;
    % Evaluate Gaussians
    if ndim == 1
      % Matlab syntax bites us
      px = exp(-0.5*   (xmm'.*(inv(cv)*xmm')))/sqrt(det(cv));
    else
      px = exp(-0.5*sum(xmm'.*(inv(cv)*xmm')))/sqrt(det(cv));
    end
    jx(:,c) = co(c)*px';
    % Save l/hood for each pt under each mix
    xlik(:,c) = pistuff*co(c)*px';
  end

  % Report data likelihood before this iteration
  olik = lik;
  lik = sum(log(sum(xlik')));
  if viz ~= 0 & rem(it,10) == 1
    disp(['Iteration=', num2str(it),' Log data likelihood = ', num2str(lik), ' delta=',num2str(lik-olik)]);
  end
  
  % Normalize rows of p(j|x) to be true posteriors
  jx = jx ./ (ones(nmix,1)*sum(jx'))';

  % Re-estimate model parameters one mixture component at a time
  for c = 1:nmix
    jxc = jx(:,c);
    sjxc = sum(jxc);
    co(c) = sjxc/ndat;
    mo(c,:) = (jxc'*x)/sjxc;
    mu = ones(ndat,1)*mo(c,:);
    xmm = x - mu;
    % Weighted expectation is scaled xT * x
    cvj =  (((jxc*ones(1,ndim)) .* xmm)' * xmm) / sjxc;
    % Store in raveled form
    vo(c,:) = cvj(1:ncov);
  end

end

if viz == -1
  if ndim == 1
    bar(histx, histn);
    gmmplot(mo,vo,co,[1],vscale*min(sqrt(vo)./co'),'r');
  else
    plot(x(:,1),x(:,2),'.b');
    gmmplot(mo,vo,co,[1 2],1,'r');
  end    
end
