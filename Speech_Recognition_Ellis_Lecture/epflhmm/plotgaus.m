function [h] = plotgaus( mu, sigma, colspec );

% PLOTGAUS Plotting of a Gaussian contour
%
%    PLOTGAUS(MU,SIGMA,COLSPEC) plots the mean and standard
%    deviation ellipsis of the Gaussian process that has mean MU
%    variance SIGMA, with color COLSPEC = [R,G,B].
%
%    If you use only PLOTGAUS(MU,SIGMA), the default color is
%    [0 1 1] (cyan).
%

if nargin < 3; colspec = [0 1 1]; end;
npts = 100;
mu = mu(:)';

stdev = sqrtm(sigma);

t = linspace(-pi, pi, npts);
t=t(:);

X = [cos(t) sin(t)] * stdev + repmat(mu,npts,1);

h(1) = line(X(:,1),X(:,2),'color',colspec,'linew',2);
h(2) = line(mu(1),mu(2),'marker','+','markersize',10,'color',colspec,'linew',2);
