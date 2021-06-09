function demoGasNewt
% demoGasNewt  Interpolate gasoline price data with Newton polynomials
%
% Synopsis:  demoGasNewt
%
% Input:     none
%
% Output:    Plot of given data and interpolating function for gas price

year =  [1986   1988   1990   1992   1994   1996 ]';    %  input data
price = [133.5  132.2  138.7  141.5  137.6  144.2]';

% --- Interpolate and plot results
y = linspace(min(year),max(year),200);  %  eval interpolant at these dates
p = newtint(year,price,y);
plot(year,price,'o',y,p,'-');
xlabel('year');  ylabel('gasoline price, (cents)');
