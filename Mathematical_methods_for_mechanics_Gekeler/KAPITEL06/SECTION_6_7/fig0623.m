function fig0623
% Figure 6.23, EULER angles

clc, clf
PHI = pi/10; THETA = pi/7; PSI = pi/8;

az = 100; el = 30;  % gut
az = 120; el = 25;  % gut

% -- ACHSEN_E  ---------
X1 = [0, 1]; Y1 = [0 0]; Z1 = [0 0]; % X-Achse
X2 = [0 0]; Y2 = [0 1]; Z2 = [0 0];  % Y-Achse
X3 = [0 0]; Y3 = [0 0]; Z3 = [0 1];  % Z-Achse
plot3(X1,Y1,Z1,'k--','linewidth',2), hold on
plot3(X2,Y2,Z2,'k--','linewidth',2), hold on
plot3(X3,Y3,Z3,'k--','linewidth',2), hold on
h = 0.1; r = 0.02;
pfeil3(X1,Y1,Z1,h,r);
pfeil3(X2,Y2,Z2,h,r);
pfeil3(X3,Y3,Z3,h,r);

% -- Kreis in Ebene -----------------------
A = pi-0.35; B = pi+0.3;A = pi -0.35;

TTA = linspace(B,2*pi,100);
TTB = linspace(0,A,100);
XX = [cos(TTA); sin(TTA); zeros(1,length(TTA))];
plot3(XX(1,:),XX(2,:),XX(3,:),'b','linewidth',2), hold on
XX = [cos(TTB); sin(TTB); zeros(1,length(TTB))];
plot3(XX(1,:),XX(2,:),XX(3,:),'b','linewidth',2), hold on

% -- Drehen um z-Achse ------------------
E1 = [1;0;0]; E2 = [0;1;0]; E3 = [0;0;1];
DD1 = drehmatrix(PHI,E3);
EN = DD1*E1;
EN1 = [-1.3*EN,1.3*EN];
plot3(EN1(1,:),EN1(2,:),EN1(3,:),'k','linewidth',2), hold on
DD2 = drehmatrix(THETA,EN);
% -- 2. Kreis -----------------------------
A = -1.6;
TTA = linspace(A,0,100);
XX1 = [cos(TTA); sin(TTA); zeros(1,length(TTA))];
XXN = DD2*XX1;
plot3(XXN(1,:),XXN(2,:),XXN(3,:),'k','linewidth',2), hold on
B = 3.45;
TTB = linspace(0,B,100);
XX1 = [cos(TTB); sin(TTB); zeros(1,length(TTB))];
XXN = DD2*XX1;
plot3(XXN(1,:),XXN(2,:),XXN(3,:),'k','linewidth',2), hold on

F3  = DD2*E3;
F31 = [zeros(3,1), F3];
DD3 = drehmatrix(PSI,F3);
F1  = DD3*EN;
F11 = [zeros(3,1),F1];
F2  = DD3*DD2*DD1*E2;
F21 = [zeros(3,1),F2];
plot3(F31(1,:),F31(2,:),F31(3,:),'k','linewidth',2), hold on
plot3(F11(1,:),F11(2,:),F11(3,:),'k','linewidth',2), hold on
plot3(F21(1,:),F21(2,:),F21(3,:),'b','linewidth',2), hold on
pfeil3(F31(1,:),F31(2,:),F31(3,:),h,r);
pfeil3(F11(1,:),F11(2,:),F11(3,:),h,r);
pfeil3(F21(1,:),F21(2,:),F21(3,:),h,r);

axis([-1 1 -1 1 -0.5 1])
% -- Ecken ------------------------
RD = 1;
RU = 0;
LL = [-RD;-RD;0];
plot3(LL(1),LL(2),LL(3),'w','markersize',3),hold on
LL = [RD;RD;RD];
plot3(LL(1),LL(2),LL(3),'w','markersize',3),hold on
view([az,el])
grid on
text(0,0.1,1,'e_3','fontsize',22)
text(0,1.1,0,'e_2','fontsize',22)
text(1,0,0.2,'e_1','fontsize',22);
text(0,1.1,0.6,'f_2','fontsize',22)
text(0,-0.6,1,'f_3','fontsize',22)
text(0.5,0.5,-0.1,'f_1','fontsize',22)
text(0,-0.18,0.5,'\theta','fontsize',22)
text(0.5,0,-0.05,'\phi','fontsize',22)
text(0.6,0.28,0.08,'\psi','fontsize',22)

%axis equal tight
xlabel('x-Achse')
ylabel('y-Achse')
zlabel('z-Achse')
% --------------------------------
axis off
