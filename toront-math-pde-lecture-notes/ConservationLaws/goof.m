y = diff(u1(:,80))/(x(2)-x(1));
[a,i] = max(abs(y));
Y = y(i-2:i+2);
X = x(i-2:i+2);
figure(1)
clf
plot(x(1:N),y,'yo-');
hold on
plot(X,Y,'rx')
figure(1)

PP = spline(X,Y);
dx = (max(X)-min(X))/1000;
xx = min(X):dx:max(X);
yy1 = ppval(PP,xx)
plot(xx,yy1,'w')
