function [h,f,xx] = plotdens(x,h,positive,kernel)
% PURPOSE: Draw a nonparametric density estimate. 
%---------------------------------------------------
% USAGE: [h f y] = pltdens(x,h,p,kernel)
%        or pltdens(x) which uses gaussian kernel default
% where:
%        x is a vector
%        h is the kernel bandwidth 
%          default=1.06 * std(x) * n^(-1/5); Silverman page 45
%        p is 1 if the density is 0 for negative values
%        k is the kernel type:
%          =1 Gaussian (default)
%        =2 Epanechnikov 
%        =3 Biweight
%        =4 Triangular
%   A jittered plot of the 
%   observations is shown below the density.
%---------------------------------------------------
% RETURNS:
%        h = the interval used
%        f = the density
%        y = the domain of support
%        plot(y,f) will produce a plot of the density
% --------------------------------------------------
% SEE ALSO hist, histo
%---------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg

x = x(:); n = length(x);
if nargin < 4, kernel = 1; end
if nargin < 3, positive = 0; end
if nargin < 2, h = []; end
if isempty(h)
   h = 1.06 * std(x) * n^(-1/5);  % Silverman page 45
end
if positive & any(x < 0)
   error('There is a negative element in X')
end
mn1 = min(x); mx1 = max(x);
mn = mn1 - (mx1-mn1)/3;
mx = mx1 + (mx1-mn1)/3;
gridsize = 256;
xx = linspace(mn,mx,gridsize)';
d = xx(2) - xx(1);
xh = zeros(size(xx));
xa = (x-mn)/(mx-mn)*gridsize;
for i=1:n
   il = floor(xa(i));
   a  = xa(i) - il;
   xh(il+[1 2]) = xh(il+[1 2])+[1-a, a]';
end
xk = [-gridsize:gridsize-1]'*d;
if kernel == 1
   K = exp(-0.5*(xk/h).^2);
elseif kernel == 2 
   K = max(0,1-(xk/h).^2/5);
elseif kernel == 3
   c = sqrt(1/7);
   K = (1-(xk/h*c).^2).^2 .* ((1-abs(xk/h*c)) > 0);
elseif kernel == 4 
   c = sqrt(1/6);
   K = max(0,1-abs(xk/h*c));
end
K = K / (sum(K)*d*n);
f = ifft(fft(fftshift(K)).*fft([xh ;zeros(size(xh))]));
f = real(f(1:gridsize));
if positive
   m = sum(xx<0);
   f(m+(1:m)) = f(m+(1:m)) + f(m:-1:1);
   f(1:m) = zeros(size(f(1:m)));
   xx(m+[0 1]) = [0 0];
end
if nargout == 0
plot(xx,f)
 if ~ishold
   hold on
   d = diff(get(get(gcf,'CurrentAxes'),'Ylim'))/100;
   plot(x,(-rand(size(x))*6-1)*d,'.')
   plot([mn mx],[0 0])
   axis([mn mx -0.2*max(f) max(f)*1.2])
   hold off
 end
end;
