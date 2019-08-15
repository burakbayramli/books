function fig0310
% Figure 3.10

clc, clf
disp(' Call first DEMO2.M, Example 3 ')
load daten Pfad
% -- feasible domain --------
X3 = [0, 2,5,5,2,0,-1,0]; Y3 = [0,-1,-1,0,3,2, 1,0];
fill(X3,Y3,'y'), hold on
% -- X-Achse -------------------
c = 0.5; d = 0.15;
X1 = [-10, 6]; Y1 = [0, 0];
arrow(X1,Y1,c,d,'k',2)
% -- Y-Achse ------------------
c = 0.5; d = 0.15;
X2 = [0, 0]; Y2 = [-6, 6];
arrow(X2,Y2,c,d,'k',2)

X    = linspace(-10,6,40);
Y    = linspace(-6,6,40);
m    = length(X);
Z    = zeros(m,m);
[U,V] = meshgrid(X,Y);
W = 0.5*(U.*U + V.*V) + 10*U + 2*V;
[C,h] = contour(U,V,W,10); hold on
%clabel(C,h,'manual');
contour(U,V,W,[-7,-7]); hold on
h = findobj('type','patch');
set(h,'linewidth',2)

% -- Restrictions -----------
A = - 4; B = 4; X = [A,B];
X = [-1,0]; Y = - X;
plot(X,Y,'k','linewidth',2), hold on
X = [-2,6]; Y = [3,3];
plot(X,Y,'k--','linewidth',2), hold on
X = [2,5]; Y = - X + 5;
plot(X,Y,'k','linewidth',2), hold on
X = [-8,0]; Y = X + 2;
plot(X,Y,'k','linewidth',2), hold on
X = [5,5]; Y = [-1,0];
plot(X,Y,'k','linewidth',2), hold on
X = [-4,6]; Y = [-1,-1];
plot(X,Y,'b--','linewidth',2), hold on
X = [-4,4]; Y = -0.5*X;
plot(X,Y,'r--','linewidth',2), hold on
X = [0,2]; Y = 0.5*X + 2;
plot(X,Y,'k','linewidth',2), hold on
plot(Pfad(1,:),Pfad(2,:),'r','linewidth',2),hold on
plot(Pfad(1,:),Pfad(2,:),'r*'),hold on
circle(-10,-2,0.15,'w')
circle(-7,-5,0.15,'w')
circle(-3,-1,0.15,'w')
circle(-1,1,0.15,'w')
circle(-4/3,2/3,0.15,'w')

text(-9.5,-2,'x_0','fontsize',22)
text(-6.5,-5.3,'x_1','fontsize',22)
text(-2.9,-1.7,'x_2,x_3','fontsize',22)
text(-4,0.5,'x_3,x_4','fontsize',22)
text(-1.8,1.8,'x_5','fontsize',22)

grid off
axis equal






