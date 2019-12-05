echo off
X = [-3:0.2:3]';
Y = [-3:0.2:3]';
[x,y]=meshgrid(X',Y') ;
func = 3*(1-x).^2.*exp(-x.^2-(y+1).^2) - 10.*(x/5-x.^3-y.^5).*exp(-x.^2-y.^2) - exp(-(x+1).^2-y.^2)/3;
func = -func;
clf
levels = exp(-5:10);
levels = [-5:0.9:10];
contour(X,Y,func,levels,'k--')
%mesh(X,Y,-func,'k--')
xlabel('x_1')
ylabel('x_2')
title('Minimization of Peaks function')
drawnow; 
hold on
plot(-0.0303,1.5455,'o')
text(-0.0303,1.5455,'Solution')
