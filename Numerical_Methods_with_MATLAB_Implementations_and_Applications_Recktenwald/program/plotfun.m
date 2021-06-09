function plotfun(ncycle,npts)
% plotfun   Plot sin(x), cos(x), and sin(x)*cos(x) for a prescribed
%           number of cycles and number of points per cycle
%
% Synopsis:  plotfun(ncycle,npts)
%
% Input:     ncycle = number of cycles (2*pi/cycle)
%            npts   = number of points to plot per cycle
%
% Output:    A plot in a separate figure window

x = linspace(0,ncycle*2*pi,ncycle*npts);    %  generate data
y1 = sin(x);
y2 = cos(x);
y3 = y1.*y2;             %  note the .* array operator

plot(x,y1,'-',x,y2,'.',x,y3,'--');
xlabel('x   (radians)')
legend('sin(x)','cos(x)','sin(x)*cos(x)')
title('Plot of simple trigonometric functions')
