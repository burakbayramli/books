close all;
clear all;
clc
Nx=5;Ny=5;x0=0;xf=1;y0=0;yf=1;
dx=(xf-x0)/Nx;dy=(yf-y0)/Ny;
x1=x0:dx/2:xf;
x2=x0:dx:xf;
y1=y0:dy:yf;
y2=dy/2:dy:yf;
x3=x0:dx:xf;
y3=y0:dy:yf;
[X1,Y1]=meshgrid(x1,y1);
[X2,Y2]=meshgrid(x2,y2);
[X3,Y3]=meshgrid(x3,y3);
u1=zeros(length(x1),length(y1));
u2=zeros(length(x2),length(y2));
u3=zeros(length(x3),length(y3));
figure
surf(x3,y3,u3','FaceColor','interp');
view(0,90)
hold on
p1=plot3(X2,Y2,u2','.','markersize',25);
p2=plot3(X1,Y1,u1','.','markersize',25);
axis([x0 xf y0 yf 0 0.01]);