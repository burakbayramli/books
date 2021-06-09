function demoGasLag
% demoGasLag  Interpolate gasoline price data with Lagrange polynomials
%
% Synopsis:  demoGasLag
%
% Input:     none
%
% Output:    Plot of given data and interpolating function for gas price

year =  [1986   1988   1990   1992   1994   1996 ]';    %  input data
price = [133.5  132.2  138.7  141.5  137.6  144.2]';

y = linspace(min(year),max(year),200);  %  eval interpolant at these dates
p = zeros(size(y));                     %  Pre-allocate p for efficiency
for i=1:length(y)
  p(i) = lagrint(year,price,y(i));      %  Interpolate to find p( y(i) )
end
plot(year,price,'o',y,p,'-');
xlabel('year');  ylabel('gasoline price, (cents)');
