function shist(x,n)
% PURPOSE: spline-smoothed plot of a histogram
% -----------------------------------------------------
% USAGE: shist(x,n)
% where: x = input matrix (or vector) 
%        n = # of bins, [default = (1/50)*size(x)]
% -----------------------------------------------------
% RETURNS: nothing, just plots the histogram
% -----------------------------------------------------

if (nargin > 2),
error('wrong # of arguments to shist');
end;

[sizex sizey] = size(x);
if (sizex > sizey), xsize = sizex; end;
if (sizey > sizex), xsize = sizey; end;
if (nargin ~= 2), nbin = (1/50)*xsize; end;
if (nargin ==2), nbin = n; end;

[t1 t2] = hist(x,nbin);
step = (abs(t2(nbin) - t2(1)))/nbin;
t2i = t2(1):step:t2(nbin);
t1i = interp1(t2,t1,t2i,'spline');
plot(t2,t1,'o',t2i,t1i);
title('Spline Smoothed Histogram');
