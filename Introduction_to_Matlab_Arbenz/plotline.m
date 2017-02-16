function plotline(x,y,s,c,n,t)
% plots the set of points (x,y) using the symbol s
% and plots the straight line c+n1*x+n2*y=0 using
% the line type defined by t
plot(x,y,s)
xrange = [min(x) max(x)];
yrange = [min(y) max(y)];
if n(1)==0, % c+n2*y=0 => y = -c/n(2)
x1=xrange(1); y1 = -c/n(2);
x2=xrange(2); y2 = y1
elseif n(2) == 0, % c+n1*x=0 => x = -c/n(1)
y1=yrange(1); x1 = -c/n(1);
y2=yrange(2); x2 = x1;
elseif xrange(2)-xrange(1)> yrange(2)-yrange(1),
x1=xrange(1); y1 = -(c+n(1)*x1)/n(2);
x2=xrange(2); y2 = -(c+n(1)*x2)/n(2);
else
y1=yrange(1); x1 = -(c+n(2)*y1)/n(1);
y2=yrange(2); x2 = -(c+n(2)*y2)/n(1);
end
plot([x1, x2], [y1,y2],t)
