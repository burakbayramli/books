function demoGasVandShift
% demoGasVandShift  Interpolate gas price data using monomials and shifted dates
%
% Synopsis:  demoGasVandShift
%
% Input:     none
%
% Output:    Plot of given data and interpolating function for gas price, along
%            with a print out of monomial coefficients

year =  [1986   1988   1990   1992   1994   1996 ]';    %  input data
price = [133.5  132.2  138.7  141.5  137.6  144.2]';

% --- Set up and solve Vandermonde system, and plot interpolant
ys = year - mean(year);
A = vander(ys);
c = A\price;
err = price - polyval(c,ys);             %  errors at the known data points
d = linspace(min(year),max(year),200);
ds = d - mean(year);  p = polyval(c,ds); %  Evaluate polynomial at shifted d values
plot(year,price,'o',d,p,'-');
xlabel('year');  ylabel('gasoline price, (cents)');

% --- Compute condition number of the interpolating polynomial
%     using C ~ abs( x*f'(x)/f(x) ) see Higham, S 1.6, p 9
avex = mean(year);
cprime = polyder(c);
polycond = avex*polyval(cprime,avex)/polyval(c,avex);

% --- Summary statistics
fprintf('condition number of A           %11.3e\n',cond(A));
fprintf('condition number of polynomial  %11.3e\n',polycond);
fprintf('max error at input prices       %11.3e\n',max(abs(err)));
fprintf('\nCoefficients of interpolating polynomial\n');
for k=1:length(c)
  fprintf('\tc(%d) = %16.6e\n',k,c(k));
end
