function demoInterp1
% demoInterp1  Use built in interp1 function on data sampled from 'humps'
%
% Synopsis:    demoInterp1
%
% Input:       none
%
% Output:      Plots showing the four types of interpolation available
%              with the built in interp1 function.  Sample data is
%              generated with the built in humps function.

xmin = 0;   xmax = 1.5;   axisScale = [xmin xmax -20 120];
x = linspace(xmin,xmax,10);
y = humps(x);
xi = linspace(min(x),max(x));

yn = interp1(x,y,xi,'nearest');
y1 = interp1(x,y,xi,'linear');
y3 = interp1(x,y,xi,'cubic');
ys = interp1(x,y,xi,'spline');

subplot(2,2,1);   plot(x,y,'o',xi,yn,'-');   axis(axisScale);
text(0.6,75,'Nearest neighbor','FontName','Times','FontSize',12)

subplot(2,2,2);   plot(x,y,'o',xi,y1,'-');   axis(axisScale);
text(0.6,75,'Piecewise linear','FontName','Times','FontSize',12)

subplot(2,2,3);   plot(x,y,'o',xi,y3,'-');   axis(axisScale);
text(0.6,75,'Piecewise cubic','FontName','Times','FontSize',12)

subplot(2,2,4);   plot(x,y,'o',xi,ys,'-');   axis(axisScale);
text(0.6,75,'Cubic spline','FontName','Times','FontSize',12)
