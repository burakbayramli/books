function bld011101
% BSkizze zu  quadratischen Formen
clc,
clf, hold on
c = 0.2;
d = 0.08;
% -- X-Achse ------------
X = [-1,3];
Y = [0,0];
arrow(X,Y,c,d,'k',2)
hold on
% -- Y- Achse ---------
X = [0,0];
Y = [-2,2];
arrow(X,Y,c,d,'k',2)
hold on
X1 = linspace(-1,1.5,30);
Y1 = X1.*X1;
plot(X1,Y1,'linewidth',2), hold on
X2 = linspace(-0.5,2.5,30);
Y2 = X2.*X2 - 2*X2;
plot(X2,Y2,'linewidth',2), hold on
X3 = [-1, 1];
Y3 = -2*X3;
plot(X3,Y3,'linewidth',2), hold on
X4 = 1;
Y4 = - 1;
circle(X4,Y4,0.05,'w'), hold on
% -- Rahmen -----
RX = [-1.2,3.2,3.2,-1.2,-1.2];
RY = [-2.2,-2.2,2.5,2.5,-2.2];
plot(RX,RY,'k','linewidth',2), hold on
text(2.75,-0.2,'x','FontSize',22)
text(0.15,1.85,'y','FontSize',22)
text(1.3,1.25,'y = x^2','FontSize',22)
text(1,-1.7,'y = - 2x','FontSize',22)
text(1.6,-0.85,'y = x^2 - 2x','FontSize',22)
grid on
axis equal tight
axis off

