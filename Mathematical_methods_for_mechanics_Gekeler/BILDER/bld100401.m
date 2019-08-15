function bld100401
% Mannigfaltigkeit
clc, clf, clear
% -- Rand ---
X = [0,20,20,0,0];
Y = [0,0,10,10,0];
plot(X,Y,'k','linewidth',2), hold on
M1 = [4,2]; R1 = 1.5;
X1 = kreis(M1,R1);
plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on
M1 = [17,3]; R1 = 1.5;
X1 = kreis(M1,R1);
plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on
M1 = [12,7]; R1 = 2.5;
X1 = kreis(M1,R1);
plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on
M1 = [9,7]; R1 = 2;
X1 = kreis(M1,R1);
plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on
% -- 1. Segment ----------------------
M1 = [6,1]; R1 = 2;
A = pi/2+ 0.41;
B = 3*pi/2- 1.3;
X1 = segment(M1,R1,A,B);
plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on
M1 = [4,2]; R1 = 1.5;
A = -pi/2;
B = pi/4-0.2;
X2 = segment(M1,R1,A,B);
%plot(X2(1,:),X2(2,:),'r','linewidth',2), hold on
X3 = [X2,X1];
fill(X3(1,:),X3(2,:),'y'), hold on
% -- 2. Segment ---------------------
M1 = [16,4]; R1 = 1.6;
A = 3*pi/2- 0.21;
B = 2*pi+0.2;
X1 = segment(M1,R1,A,B);
plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on
M1 = [17,3]; R1 = 1.5;
A = pi/2-0.4;
B = pi+0.4;
X2 = segment(M1,R1,A,B);
%plot(X2(1,:),X2(2,:),'b','linewidth',2), hold on
X3 = [X1,X2];
fill(X3(1,:),X3(2,:),'y'), hold on
% -- 3. Segment ---------------------
M1 = [9,7]; R1 = 2;
A = -pi/2 + 0.6;
B = pi/2-0.6;
X1 = segment(M1,R1,A,B);
plot(X1(1,:),X1(2,:),'r','linewidth',2), hold on
M1 = [12,7]; R1 = 2.5;
A = pi/2+0.8;
B = 3*pi/2-0.8;
X2 = segment(M1,R1,A,B);
plot(X2(1,:),X2(2,:),'k','linewidth',2), hold on
X3 = [X1,X2];
fill(X3(1,:),X3(2,:),'y'), hold on
% Achsen ------------
X = [1.5,6];
Y = [1.5,1.5];
plot(X,Y,'k--','linewidth',1.5), hold on
X = [3,3];
Y = [0.5,4];
plot(X,Y,'k--','linewidth',1.5), hold on
X = [15,19.5];
Y = [2,2];
plot(X,Y,'k--','linewidth',1.5), hold on
X = [18,18];
Y = [1,5];
plot(X,Y,'k--','linewidth',1.5), hold on
% -- Pfeile --------------
c = 0.6; d = 0.2;
X = [6,15]; Y = [2,2.5];
arrow(X,Y,c,d,'k',2)
X = [15,6]; Y = [3,2.5];
arrow(X,Y,c,d,'k',2)
d = 0.3;
X = [15.5,13]; Y = [4.1,6];
arrow(X,Y,c,d,'k',2)
X = [13,15.7]; Y = [6.4,4.4];
arrow(X,Y,c,d,'k',2)
X = [8,4.6]; Y = [6.3,3.7];
arrow(X,Y,c,d,'k',2)
X = [4.8,8.3]; Y = [3.4,6];
arrow(X,Y,c,d,'k',2)
axis equal, grid on
text(8,7,'U_1','fontsize',18)
text(12,7.3,'U_2','fontsize',18)
text(4.3,5.5,'\Psi_1','fontsize',18)
text(6.4,4,'\Phi_1','fontsize',18)
text(13.8,4,'\Phi_2','fontsize',18)
text(15.2,5.6,'\Psi_2','fontsize',18)
text(9,1.2,'\Psi_1^{-1} \circ \Psi_2','fontsize',18)
text(0.3,0.9,'R^n(\xi)','fontsize',18)
text(17.3,1,'R^n(\zeta)','fontsize',18)
text(9,3.5,'\Psi_2^{-1} \circ \Psi_1','fontsize',18)
%grid on
axis off

function X = kreis(M,R)
TT = linspace(0,2*pi,60);
X1 = M(1) + R*cos(TT);
X2 = M(2) + R*sin(TT);
X = [X1;X2];

function X = segment(M,R,A,B)
TT = linspace(A,B,60);
X1 = M(1) + R*cos(TT);
X2 = M(2) + R*sin(TT);
X = [X1;X2];

