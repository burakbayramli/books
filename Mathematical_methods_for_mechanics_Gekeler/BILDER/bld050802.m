% Abb. 156, Fortsetzung nach Allgower-Georg
clf
clear
r = 0.13;c = 0.5; d = 0.2;
% -- Kurve ----------------
a = 0.5;
R = 10;
T = linspace(pi,pi/4,40);
X1 = R*cos(T);
Y1 = - R + R*sin(T);
plot(X1,Y1,'linewidth',2), hold on
% -- Punkte und Tangenten -------
PHI1 = pi-0.2;
X5 = [R*cos(PHI1); -R + R*sin(PHI1)];
T5 = [R*sin(PHI1);-R*cos(PHI1)];
TT5 = [R*cos(PHI1);R*sin(PHI1)];
X5T = X5 + 0.3*T5;
X5E = X5 + 0.5*T5;
X5F = X5E - 0.3*TT5;
plot([X5(1),X5E(1)], [X5(2),X5E(2)],'k'), hold on
arrow_6(X5,X5T,c,d,'k',2)
arrow_6(X5E,X5F,c,d,'k',2)
% -- Viertelkreis
RAD = 0.07;
PHI = -pi/2;
X5H = X5E - RAD*TT5;
[X,Y] = segm(X5E,X5H,PHI,1);
plot(X,Y,'k','linewidth',1), hold on
% --------------------------------
PHI2 = pi - 0.72;
X6 = [R*cos(PHI2); -R + R*sin(PHI2)];
T6 = [R*sin(PHI2);-R*cos(PHI2)];
TT6 = [R*cos(PHI2);R*sin(PHI2)];
X6T = X6 + 0.3*T6;
X6E = X6 + 0.5*T6;
X6F = X6E - 0.3*TT6;
plot([X6(1),X6E(1)], [X6(2),X6E(2)],'k'), hold on
arrow_6(X6,X6T,c,d,'k',2)
arrow_6(X6E,X6F,c,d,'k',2)
% -- Viertelkreis
RAD = 0.07;
PHI = -pi/2;
X6H = X6E - RAD*TT6;
[X,Y] = segm(X6E,X6H,PHI,1);
plot(X,Y,'k','linewidth',1), hold on
% -----------------------------------
PHI3 = pi - 1.24;
X7 = [R*cos(PHI3); -R + R*sin(PHI3)];
T7 = [R*sin(PHI3);-R*cos(PHI3)];
TT7 = [R*cos(PHI3);R*sin(PHI3)];
X7T = X7 + 0.3*T7;
X7E = X7 + 0.8*T7;
X7F = X7E - 0.3*TT7;
X7G = X7E - 0.4*TT7;
% -- Viertelkreis
RAD = 0.07;
PHI = -pi/2;
X7H = X7E - RAD*TT7;
[X,Y] = segm(X7E,X7H,PHI,1);
plot(X,Y,'k','linewidth',1), hold on
% -----------------------------------
plot([X7(1),X7E(1)], [X7(2),X7E(2)],'k'), hold on
plot([X7E(1),X7G(1)], [X7E(2),X7G(2)],'k'), hold on
arrow_6(X7,X7T,c,d,'k',2)
arrow_6(X7E,X7F,c,d,'k',2)

PHI4 = pi - 2.166;
X8 = [R*cos(PHI4); -R + R*sin(PHI4)];
T8 = [R*sin(PHI4);-R*cos(PHI4)];
X8T = X8 + 0.3*T8;
arrow_6(X8,X8T,c,d,'k',2)

POLY = [X5(1), X6(1), X7(1), X8(1);
        X5(2), X6(2), X7(2), X8(2)];
%plot(POLY(1,:),POLY(2,:),'k--','linewidth',2), hold on

circle(X5(1),X5(2),r,'w')
circle(X6(1),X6(2),r,'w')
circle(X7(1),X7(2),r,'w')
circle(X8(1),X8(2),r,'w')
circle(X5E(1),X5E(2),r,'w')
circle(X6E(1),X6E(2),r,'w')
circle(X7E(1),X7E(2),r,'w')

text(-9.3,-8.3,'x_{k-2}','fontsize',22)
text(-7.5,-4.4,'x_{k-1}','fontsize',22)
text(-3.6,-1.5,'x_k','fontsize',22)
text(5.9,-1.2,'x_{k+1}','fontsize',22)
text(-5,-6.7,'t_{k-2}','fontsize',22)
arrow_6([-5.4,-6.6],[-8.9,-6.6],0.35,0.15,'k',1)
text(-7,3,'t_{k-1}','fontsize',22)
arrow_6([-6.8,2.1],[-6.8,-2.2],0.35,0.15,'k',1)
text(-2.3,3,'t_k','fontsize',22)
arrow_6([-2.1,2.1],[-2.1,0.2],0.3,0.1,'k',1)

RAHMENX = [-11,8.5,8.5,-11,-11];
RAHMENY = [-11,-11,4,4,-11];
plot(RAHMENX,RAHMENY,'k','linewidth',2)

axis equal tight
%grid on
axis off
