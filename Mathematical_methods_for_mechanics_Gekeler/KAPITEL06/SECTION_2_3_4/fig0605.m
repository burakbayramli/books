function fig0605
% Figure 6.5, Motion in central field

disp(' Call first demo2.m if not done ') 
clf
load daten3 T X Y
c   = 0.1; d = 0.05;
[XA,YA] = pol2cart(Y(:,1),Y(:,2));
[XB,YB] = pol2cart(X(1),X(2));
TT = linspace(0,2*pi,80);
r1 = 0.18;
r2 = 1.5;
X1 = r1*cos(TT); Y1 = r1*sin(TT);
X2 = r2*cos(TT); Y2 = r2*sin(TT);
plot(XA,YA,'k','linewidth',2), hold on
plot(X1,Y1,'r'), hold on
plot(X2,Y2,'k','linewidth',2), hold on
RR = 1.9;
X3 = [0;RR];
Y3 = [0;0];
PHI = 0.925;
X4 = [0;RR*cos(PHI)];
Y4 = [0;RR*sin(PHI)];
arrow(X3,Y3,c,d,'k',1)
arrow(X4,Y4,c,d,'k',1)
% -- Bezeichnung ----------
RS = 1.8;
TT = linspace(0,PHI/3+0.05, 20);
X5 = RS*cos(TT); Y5 = RS*sin(TT);
plot(X5,Y5,'k','linewidth',2), hold on
TT = linspace(2*PHI/3-0.05,PHI, 20);
X6 = RS*cos(TT); Y6 = RS*sin(TT);
plot(X6,Y6,'k','linewidth',2), hold on
c    = 0.17; d = 0.07;
X7   = [RS*cos(PHI);RS*sin(PHI)];
PSI1 = -pi/4+pi;
arrow_8(c,d,X7,PSI1)
X8   = [1.8;0];
PSI2 = -1.5;
arrow_8(c,d,X8,PSI2)
rr  = 0.05;
circle(0,0,rr,'w')
circle(XB,YB,rr,'w')
text(1.5,0.8,'\Delta\Phi','fontsize',22)
% -- Rahmen  ---------
XR = [-1.7,1.9,1.9,-1.7,-1.7];
YR = [-1.7,-1.7,1.7,1.7,-1.7];
plot(XR,YR,'k','linewidth',2),hold on
grid off
axis equal tight
axis off
