function [xe,ye] = errell(xm,ym,cov,nsig)
%ERRELL usage:
%	        [xe,ye] = errell(xm,ym,cov)
%	     or [xe,ye] = errell(xm,ym,cov,nsig)
%       computes an error ellipse centered at(xm,ym) using
%       the covariance matrix cov associated with xm,ym.
%       If nsig is specified, it indicates the sigma level
%       (e.g. if nsig = 1 you get 1 sigma ellipse, if nsig = 2
%       you get the 2-sigma or ~86% conf. ellipse).
%       default value for nsig = 1. The vectors xe & ye
%       describe the ellipse centered about (xm,ym).

%Copyright (c)  Mike Bevis 1997

ne = 100;  % no. pts in ellipse is 2*ne
if length(xm) ~= 1 | length(ym) ~= 1
   error(' input variables xm, ym should be scalars')
end
[ncr,ncc] = size(cov);
if ncr ~= 2 | ncc ~= 2
   error(' input variable cov must be 2 x 2 matrix')
end
if nargin == 3
   nsig = 1;	% default is one sigma level ellipse
end
[v,d] = eig(cov);
std1 = sqrt(d(1,1));  std2 = sqrt(d(2,2));
if std1 < std2
  z1 = linspace(-std1,+std1,ne);
  z2 = sqrt( d(2,2).*( ones(1,ne)- (z1/std1).^2 ) );
  dxy = v*[z1 fliplr(z1);z2 -z2];
else
  z2 = linspace(-std2,+std2,ne);
  z1 = sqrt( d(1,1).*( ones(1,ne)- (z2/std2).^2 ) );
  dxy = v*[z1 -z1 ;z2 fliplr(z2)];
end
dx = dxy(1,:); dy = dxy(2,:);
xe = xm*ones(1,2*ne) + nsig*dx;
ye = ym*ones(1,2*ne) + nsig*dy;
%%%%%%%%%%end errell.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
