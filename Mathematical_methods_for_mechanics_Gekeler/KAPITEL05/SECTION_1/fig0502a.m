function fig0502a
% Figure 5.2, c = 1
 
clc, clf, hold on
ee = 0.01;
% -- Ecken ---------------
plot(-1-ee,-1-ee,'k.'), hold on
plot(1.5+ee,1+ee,'k.'), hold on
axis equal tight, axis manual
% -- Rahmen ---------------
X = [-1,1.5,1.5,-1,-1]; Y = [-1,-1,1,1,-1];
plot(X,Y,'k','linewidth',2), hold on
[X,Y] = meshgrid(-1:0.05:1.5,-1:0.05:1);
Z = 0.5*Y.*(X - Y.^2);
C = 1; MU = 1;
U = MU*X  - 3*C*Y.^2;
V = - Y;
streamslice(X,Y,U,V); hold on
% -- Achsen ------------------
c = 0.1; d = 0.04;
X = [-1,1.5]; Y = [0,0]; 
arrow(X,Y,c,d,'k',2)
X = [0,0]; Y = [-1,1]; 
arrow(X,Y,c,d,'k',2)

Y = linspace(-1,1,20); X = Y.*Y; 
plot(X,Y,'k','linewidth',2), hold on
plot(X,Y,'k','linewidth',2), hold on
text(0.08,0.9,'x','fontsize',30)
text(1.3,-0.1,'\mu','fontsize',30)

grid on
axis off
