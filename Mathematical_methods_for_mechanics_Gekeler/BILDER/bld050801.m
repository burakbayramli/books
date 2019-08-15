% Abb. 155, Fortsetzung nach Rheinboldt
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
X5T = X5 + 0.3*T5;
X5E = X5 + 0.415*T5;
X5F = [X5(1); X5(2) + 3];
plot([X5(1),X5E(1)], [X5(2),X5E(2)],'k'), hold on
arrow_6(X5,X5T,c,d,'k',2)
arrow_7(X5,X5F,c,d,'k',2)

PHI2 = pi - 0.65;
X6 = [R*cos(PHI2); -R + R*sin(PHI2)];
T6 = [R*sin(PHI2);-R*cos(PHI2)];
X6T = X6 + 0.3*T6;
X6E = X6 + 0.41*T6;
X6F = [X6(1)+ 3; X6(2)];
plot([X6(1),X6E(1)], [X6(2),X6E(2)],'k'), hold on
plot([X5E(1),X6(1)],[X5E(2),X6(2)],'k','linewidth',2), hold on
arrow_6(X6,X6T,c,d,'k',2)
arrow_7(X6,X6F,c,d,'k',2)

PHI3 = pi - 1.2;
X7 = [R*cos(PHI3); -R + R*sin(PHI3)];
T7 = [R*sin(PHI3);-R*cos(PHI3)];
X7T = X7 + 0.3*T7;
X7E = X7 + 0.93*T7; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
X7F = [X7(1)+ 3; X7(2)];
plot([X7(1),X7E(1)], [X7(2),X7E(2)],'k'), hold on
plot([X6E(1),X7(1)],[X6E(2),X7(2)],'k','linewidth',2), hold on
arrow_6(X7,X7T,c,d,'k',2)
arrow_7(X7,X7F,c,d,'k',2)

PHI4 = pi - 2.1;
X8 = [R*cos(PHI4); -R + R*sin(PHI4)];
T8 = [R*sin(PHI4);-R*cos(PHI4)];
X8T = X8 + 0.3*T8;
X8E = X8 + 0.5*T8;
X8F = [X8(1); X8(2) - 3];
%plot([X8(1),X8E(1)], [X8(2),X8E(2)],'k'), hold on
plot([X7E(1),X8(1)],[X7E(2),X8(2)],'k','linewidth',2), hold on
arrow_6(X8,X8T,c,d,'k',2)
arrow_7(X8,X8F,c,d,'k',2)

POLY = [X5(1), X6(1), X7(1), X8(1);
        X5(2), X6(2), X7(2), X8(2)];
plot(POLY(1,:),POLY(2,:),'k--','linewidth',2), hold on

circle(X5(1),X5(2),r,'w')
circle(X6(1),X6(2),r,'w')
circle(X7(1),X7(2),r,'w')
circle(X8(1),X8(2),r,'w')
circle(X5E(1),X5E(2),r,'w')
circle(X6E(1),X6E(2),r,'w')
circle(X7E(1),X7E(2),r,'w')


text(-9.3,-8.3,'x_{k-2}','fontsize',22)
text(-7.8,-4.7,'x_{k-1}','fontsize',22)
text(-3.6,-1.5,'x_k','fontsize',22)
text(5.5,-1,'x_{k+1}','fontsize',22)
text(-4.7,-4.1,'e_{k-1}','fontsize',22)
text(-9.5,1,'e_{k-2}','fontsize',22)
arrow_6([-9,0],[-9.6,-4.7],0.3,0.1,'k',1)
text(-5,-7,'t_{k-2}','fontsize',22)
arrow_6([-5.4,-6.6],[-8.8,-5.6],0.3,0.1,'k',1)
text(-7,3,'t_{k-1}','fontsize',22)
arrow_6([-7,2.1],[-7,-2.4],0.3,0.1,'k',1)
text(-2.8,3,'t_k','fontsize',22)
arrow_6([-2.5,2.1],[-2.5,0],0.3,0.1,'k',1)
text(-2,-4.1,'e_k','fontsize',2)
arrow_6([-1.8,-3.3],[-1.8,-1],0.3,0.1,'k',1)
text(2.3,-3.5,'-e_{k+1}','fontsize',22)

RAHMENX = [-11,8,8,-11,-11];
RAHMENY = [-11,-11,4,4,-11];
plot(RAHMENX,RAHMENY,'k','linewidth',2)

axis equal tight
grid on
axis off
