function compInterp(npts)
% compInterp  Compare flops for interpolation with different polynomial bases
%
% Synopsis:  compInterp(npts)
%
% Input:     npts = (optional) number of points at which to evaluate
%                   the interpolant.  Default:  npts = 200
%
% Output:    Table of flop counts

if nargin<1,  npts=200;  end

year =  [1986   1988   1990   1992   1994   1996 ]';    %  input data
price = [133.5  132.2  138.7  141.5  137.6  144.2]';

y = linspace(min(year),max(year),npts);  %  eval interpolant at these dates

% --- monomial basis
flops(0);
ys = year - mean(year);    %   Shift year to improve condition
A = vander(ys);
c = A\price;
p = polyval(c,y-mean(year));
fmono = flops;

% --- Lagrange basis
flops(0);
p = zeros(size(y));                  %  Pre-allocate p for efficiency
for i=1:length(y)
  p(i) = lagrint(year,price,y(i));   %  Evaluate polynomial at y(i)
end
flagr = flops;

% --- Newton basis
flops(0);
p = newtint(year,price,y);
fnewt = flops;

fprintf('\nInterpolate gasoline prices at %d points\n\n',npts);
fprintf('  Basis        flops\n');
fprintf('Monomial    %8d\n',fmono);
fprintf('Lagrange    %8d\n',flagr);
fprintf('Newton      %8d\n',fnewt);
