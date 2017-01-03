function [cross1,cross2,min1,min2] = extract(x,u)



y = diff(u)/(x(2)-x(1));
[a,i] = max(abs(y));

Y = y(i-2:i+2);
X = x(i-2:i+2);
PP = spline(X,Y);
dx = (max(X)-min(X))/1000;
xx = min(X):dx:max(X);
yy1 = ppval(PP,xx);
min1 = max(abs(yy1));
%
Y = y(i-3:i+3);
X = x(i-3:i+3);
PP = spline(X,Y);
dx = (max(X)-min(X))/1000;
xx = min(X):dx:max(X);
yy2 = ppval(PP,xx);
min2 = max(abs(yy2));

y = u;
alpha = 1/2;
i = min(find(y<alpha))-1;
Y = y(i-2:i+2);
X = x(i-2:i+2);
PP = spline(X,Y);
dx = (max(X)-min(X))/1000;
xx = min(X):dx:max(X);
yy1 = ppval(PP,xx);
cross1 = find_thresh(xx,yy1,alpha);

Y = y(i-3:i+3);
X = x(i-3:i+3);
PP = spline(X,Y);
dx = (max(X)-min(X))/1000;
xx = min(X):dx:max(X);
yy2 = ppval(PP,xx);
cross2 = find_thresh(xx,yy2,alpha);


