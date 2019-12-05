echo off

clf;
%fig=colordef(gcf, 'white');
%figure(fig)
%set(fig,'defaultaxesxgrid','off','defaultaxesygrid','off','defaultaxeszgrid','off','defaultaxesbox','off')

X = [-2:0.125:2]';
Y = [-1:0.125:3]';
[x,y]=meshgrid(X',Y') ;
func = 100.*(y-x.*x).^2 + (1-x).^2;
levels = exp(3:20);
contour(X,Y,func,levels,'k--')
xlabel('x_1')
ylabel('x_2')
title('Minimization of Rosenbrock function')
drawnow; 
hold on
plot(1,1,'o')
text(1,1,'Solution')
