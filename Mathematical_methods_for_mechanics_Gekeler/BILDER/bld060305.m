% Effektive Potentialenergie
% V(r) = -1/r + 1/(2r^2)
clf
c = 0.2;
d = 0.07;
X = [-0.2,4];
Y = [0, 0];
arrow(X,Y,c,d,'k',2)
hold on
c = 0.2;
d = 0.05;
X = [0, 0];
Y = [-0.7,2];
arrow(X,Y,c,d,'k',2)
hold on
R = linspace(0.3,4,80);
M2 = 0.5;
V = -1./R + 1./(2*R.*R);
plot (R,V,'k','linewidth',2), hold on
R1 = 2/3;
R2 = 2/5;
X = [R1,R1];
Y = [-0.5,0.3];
plot(X,Y,'k--','linewidth',2), hold on
X = [R2,R2];
Y = [-0.5,1.3];
plot(X,Y,'k--','linewidth',2), hold on
X = [-0.1,2.8]; E = 5/8; Y = [E,E];
plot(X,Y,'k--','linewidth',2), hold on
X = [-0.1,2.8]; E = -3/8; Y = [E,E];
plot(X,Y,'k--','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%
text(3,5/8,'E = 5/8','Fontsize',22)
text(2.9,-3.3/8,'E = - 3/8','Fontsize',22)
text(3.8,0.2,'r','Fontsize',22)
text(0.4,1.9,'V(r)','Fontsize',22)
text(0.7,0.2,'r = 2/3','Fontsize',22)
text(0.45,1.2,'r = 2/5','Fontsize',22)
% -- Rahmen ----
XR = [-0.2,4,4,-0.2,-0.2];
YR = [-0.7,-0.7,2.2,2.2,-0.7];
plot(XR,YR,'k','linewidth',2)
axis equal tight, grid on
axis off
