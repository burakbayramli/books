function [rslt,xx,yy]=gmmgridsamp(ms,vs,cs,rng,stps)
% [rr,xx,yy]=gmmgridsamp(ms,vs,cs,range,steps) sample GMM values on a grid
%   ms,vs,cs define a Gaussian mix model's means, raveled vars and mix
%   priors.  Vary each input dimension 
%   over <steps> values between the corresponding pair of 
%   numers in <range> and return likelihood values..
% 2001-02-09 dpwe@ee.columbia.edu
% $Header: $

xmin = rng(1);
xmax = rng(2);
ymin = rng(3);
ymax = rng(4);

xx = (xmin:((xmax-xmin)/(stps-1)):xmax)';
yy = (ymin:((ymax-ymin)/(stps-1)):ymax)';

rslt = zeros(stps, stps);

npts = stps;

for i = 1:npts;
  xxx = xx(i);
  pts = [(xxx*ones(npts,1)),yy];
  out = gmmval(pts,ms,vs,cs);
  rslt(:,i) = out;
end

%contour(xx,yy,rslt);
