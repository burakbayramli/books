function bld110401
clc, clear,
clf, hold on
xi = pi/3;
% -- Rahmen -----------------------
LU = [-0.5,-0.5]; RO = [2,1.375];
XR = [LU(1),RO(1),RO(1),LU(1),LU(1)];
YR = [LU(2),LU(2),RO(2),RO(2),LU(2)];
plot(XR,YR,'k','linewidth',2), hold on
delta = 0.01;
plot(LU(1)-delta,LU(2)-delta,'w.','markersize',3), hold on
plot(RO(1)+delta,RO(2)+delta,'w.','markersize',3), hold on
axis equal tight, axis manual
grid on
c = 0.1; d = 0.03;
X1 = [-0.4,2; 0,0];
arrow(X1(1,:),X1(2,:),c,d,'k',2), hold on

X1 = [0, cos(xi); 0, sin(xi)];
plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on
Tang = [sin(xi); -cos(xi)];
X3 = [X1(:,2),X1(:,2) + xi*Tang];
plot(X3(1,:),X3(2,:),'k','linewidth',2), hold on
X4 = [zeros(2,1),X3(:,2)];
plot(X4(1,:),X4(2,:),'k','linewidth',2), hold on
aa = 0.5;
X5 = [X3(:,2) - aa*X1(:,2),X3(:,2) + aa*X1(:,2)];
plot(X5(1,:),X5(2,:),'k','linewidth',2), hold on
aa = 0.2;
[X6,Y6] = segm(X3(:,2),X3(:,2)+aa*X1(:,2),pi/2,1);
plot(X6,Y6,'k:','linewidth',2), hold on

% -- Evolvente --------------
xi1 = 2;
XI = linspace(0,xi1,40);
RR = sqrt(1 + XI.*XI);
PSI = XI - atan(XI);
XXL = [RR.*cos(PSI); RR.*sin(PSI)];
plot(XXL(1,:),XXL(2,:),'k','linewidth',2), hold on


TT = linspace(-pi/8,5*pi/8,80);
X2 = cos(TT); Y2 = sin(TT);
plot(X2,Y2,'k','linewidth',2), hold on

RR = 0.03;
circle(0,0,RR,'w')
circle(X1(1,2),X1(2,2),RR,'w')
circle(X3(1,2),X3(2,2),RR,'w')
circle(1,0,RR,'w')

rr = 0.25;
TT = linspace(0,pi/3,20);
X1 = rr*cos(TT); Y1 = rr*sin(TT);
plot(X1,Y1,'k:','linewidth',2), hold on
rr = 0.6;
invphi = tan(pi/4) - pi/4;
TT = linspace(0,invphi,20);
X1 = rr*cos(TT); Y1 = rr*sin(TT);
plot(X1,Y1,'k:','linewidth',2), hold on
rr = 0.35;
TT = linspace(invphi,pi/3,20);
X1 = rr*cos(TT); Y1 = rr*sin(TT);
plot(X1,Y1,'k:','linewidth',2), hold on
A = [cos(xi),sin(xi)];
B = (1-rr)*A; PHI = pi/2;
[X,Y] = segm(A,B,PHI,1);
plot(X,Y,'k:','linewidth',2), hold on
text(0.15,0.5,'r','fontsize',24)
text(1,0.75,'r\xi','fontsize',24)
text(0.4,-0.2,'inv \phi','fontsize',24)
text(0.09,0.08,'\xi','fontsize',24)
text(0.25,0.25,'\phi','fontsize',24)
X1 = [0.55,0.5]; Y1 = [-0.15,0.06];
arrow(X1,Y1,c,d,'k',1), hold on
axis off
