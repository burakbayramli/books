function bld060603
% Kreisel: Potentielle Energie
clf, clc
c = 0.1; d = 0.05;
% -- Achsen ---------------
X1 = [-1,0.7]; Y1 = [0, 0];
plot(X1,Y1,'k','linewidth',3), hold on
X2 = [0,0]; Y2 = [0,2];
arrow(X2,Y2,c,d,'k',2)
THETA = 1.1*3*pi/5;
X3 = [0,2*cos(THETA)]; Y3 = [0,2*sin(THETA)];
X3T = [-Y3(2),X3(2)];
dd = 0.2; ee = 0.65;
DREIECK = [0,ee*X3(2)+dd*X3T(1),ee*X3(2)-dd*X3T(1),0;
           0,ee*Y3(2)+dd*X3T(2),ee*Y3(2)-dd*X3T(2), 0];
fill(DREIECK(1,:),DREIECK(2,:),'y'), hold on
arrow(X3,Y3,c,d,'k',2)
X4 = [0,0]; Y4 = [0,1.5];
THETA = THETA - pi/2;
[X5,Y5] = segm(X4,Y4,THETA,0);
plot(X5,Y5,'k'), hold on
cc = 0.5; rr = 0.05;
X6 = cc*X3(2); Y6 = cc*Y3(2);
X7 = [X6,X6]; Y7 = [Y6,0.2];
c = 0.15; d = 0.1;
arrow(X7,Y7,c,d,'k',4)
X8 = [0,X6]; Y8 = [Y6,Y6];
plot(X8,Y8,'k'), hold on
circle(X6,Y6,rr,'k')

text(0.08,1.85,'e_3','Fontsize',22)
text(-0.85,1.7,'f_3','Fontsize',22)
text(-0.4,1.6,'\theta','Fontsize',22)
text(0.08,0.87,'l cos \theta','Fontsize',22)
text(-0.25,0.6,'l','Fontsize',22)
text(-0.8,0.2,'mg','Fontsize',22)
text(0.06,0.12,'O','Fontsize',22)

% -- Rahmen ------------------
X9 = [-1.1,0.8]; Y9 = [-0.1,2.1];
plot(X9,Y9,'w.','markersize',3)
axis equal tight
grid on
axis off
