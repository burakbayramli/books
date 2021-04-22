clear all
clf, close all
clc
ex=1
if ex==1,
    A = [2 1; 1 2];
    b = [4 5]';
    x0 = [1 1.75]';
    xmin=0.7;xmax=1.4;dx=0.01;  ymin=1.7;ymax=2.4;dy=0.01;
else ex==2,
    A = [3 2; 2 6];
    b = [2 -8]';
    x0=[-2 -2]';
    xmin=-4;xmax=6;dx=0.01;  ymin=-6;ymax=4;dy=0.01;
end
i = 1;
xsd(:,1) = x0;
residual = 1;
while (residual > 0.00001)
    r = b - A*(xsd(:,i));
    i = i+1;
    xsd(:,i) = xsd(:,i-1)+ (r'*r)*r/(r'*A*r);
    residual = r'*r;
end
i = 1;
xcg(:,1) = x0;
v = b-(A*xcg(:,1));
r = v;
residual = 1;
while (residual > 0.00001)
    alpha = (v'*r) / (v'*A*v);
    i = i+1;
    xcg(:,i) = xcg(:,i-1) + alpha *v;
    r = r - alpha * A*  v;
    beta = -(v'*A*r)/(v'*A*v);
    v = r + beta * v;
    residual = r'*r;
end
%f = @(x,y) [x; y]' * A * [ x; y]) - b'.*[x; y] ;
f = @(x,y) ((x.^2)*A(1,1)+(y.^2)*A(2,2)+(x.*y)*(A(1,2)+A(2,1)))./2-(b(1).*x)-(b(2).*y);

plot(xcg(1,:),xcg(2,:),'-r+','LineWidth',1)
hold on
plot(xsd(1,:),xsd(2,:),'-g','LineWidth',1)

[X Y] = meshgrid([xmin:dx:xmax],[ymin:dy:ymax]);
contour(X,Y,f(X,Y),f(xsd(1,:),xsd(2,:)),'LineWidth',1)
% contour(X,Y,f(X,Y),f(xsd(1,:),xsd(2,:)))
xlim([xmin,xmax])
ylim([ymin,ymax])
xlabel('X')
ylabel('Y')
grid on;
axis equal

