%ellipsoid_sample.m
clear all,clf
global a b c
a=4;b=2;c=1;h=2*pi/100;
[v1,v2]=meshgrid([0:h:2*pi],[0:h:pi]);
X1=@(v1,v2)(a*sin(v2).*cos(v1));
X2=@(v1,v2)(b*sin(v2).*sin(v1));
X3=@(v1,v2)(c*cos(v2));
for i=1:10^3
    [V1,V2]=rand_ellipsoid;
    data(i,:)=[V1,V2];
end
hold on
surf(X1(v1,v2),X2(v1,v2),X3(v1,v2),'LineStyle','none'), axis equal,
colormap(gray)
plot3(X1(data(:,1),data(:,2)),...
X2(data(:,1),data(:,2)),X3(data(:,1),data(:,2)),...
'.','MarkerSize',10), axis equal
alpha(0.8)
