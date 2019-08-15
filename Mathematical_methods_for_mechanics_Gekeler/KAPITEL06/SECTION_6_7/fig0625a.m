function fig0625a
% Figure 6.25a, polhode and herpolhode cone

clc, clf
PHI = pi/10; THETA = pi/7; PSI = pi/8;

az = 100; el = 30;  % gut
az = 120; el = 25;  % gut
% -- ACHSEN_E  ---------
h = 0.2; r = 0.05;
X1 = [0, 0]; Y1 = [0, 0]; Z1 = [0, 2.7]; % d im Raum
plot3(X1,Y1,Z1,'k','linewidth',2), hold on
pfeil3(X1,Y1,Z1,h,r);

NN = 100;
TT = linspace(0,2*pi,NN);
% -- Nutationskegel -----------------------------
H1 = 1.8; R1 = 1.2;
K1 = [R1*cos(TT);R1*sin(TT); H1*ones(1,NN)];
plot3(K1(1,:),K1(2,:),K1(3,:),'k','linewidth',1), hold on
PHI = az*pi/180;
PSI = el*pi/180;
P1 = [0, R1*cos(PHI-PSI); 0, R1*sin(PHI-PSI); 0, H1];
plot3(P1(1,:),P1(2,:),P1(3,:),'k','linewidth',1), hold on
P1 = [0, R1*cos(PHI+pi+PSI); 0, R1*sin(PHI+pi+PSI); 0, H1];
P1 = 1.4*P1;
plot3(P1(1,:),P1(2,:),P1(3,:),'k','linewidth',2), hold on
pfeil3(P1(1,:),P1(2,:),P1(3,:),h,r);

% -- Herpolhodiekegel -----------------------------
H1 = 1; R1 = 0.3;
K1 = [R1*cos(TT);R1*sin(TT); H1*ones(1,NN)];
plot3(K1(1,:),K1(2,:),K1(3,:),'k','linewidth',2), hold on
PHI = az*pi/180;
PSI = el*pi/180;
P2 = [0, R1*cos(PHI-PSI); 0, R1*sin(PHI-PSI); 0, H1];
plot3(P2(1,:),P2(2,:),P2(3,:),'k','linewidth',2), hold on
P2 = [0, R1*cos(PHI+pi+PSI); 0, R1*sin(PHI+pi+PSI); 0, H1];
plot3(P2(1,:),P2(2,:),P2(3,:),'k','linewidth',2), hold on
P3 = 3*P2;
plot3(P3(1,:),P3(2,:),P3(3,:),'k','linewidth',2), hold on
pfeil3(P3(1,:),P3(2,:),P3(3,:),h,r);
% -- Polhodiekegel --------------------------------
POL = zeros(3,NN);
PSI = [1:NN]*2*pi/NN;
for I = 1:NN
   POL(:,I) = drehmatrix(PSI(I),P1(:,2))*P2(:,2);
end
plot3(POL(1,:),POL(2,:),POL(3,:),'k','linewidth',2),hold on
PSI = pi;
P3 = drehmatrix(PSI,P1(:,2))*P2(:,2);
P3 = [zeros(3,1), P3];
plot3(P3(1,:),P3(2,:),P3(3,:),'k','linewidth',2), hold on
grid on
text(0,0.1,2.9,'d','fontsize',22)
text(0,-0.8,2.8,'\Omega','fontsize',22)
text(1,-1,2.5,'f_1','fontsize',22);
text(0,0.6,2.6,'precession cone','fontsize',22)
text(0,0.45,1,'herpolhode cone','fontsize',22)
text(0.8,-2.5,0.25,'polhode cone','fontsize',22)
%text(0,-0.18,0.5,'\theta','fontsize',22)
%text(0.5,0,-0.05,'\phi','fontsize',22)
%text(0.6,0.28,0.08,'\psi','fontsize',22)
% -- Rand ----------------------------
LU = [-1,-1,0];
plot3(LU(1),LU(2),LU(3),'w'), hold on
RO = [1,1,2.7];
plot3(RO(1),RO(2),RO(3),'w'), hold on
view(az,el)
axis equal tight
xlabel('e_1-Achse','fontsize',18)
zlabel('e_3-Achse','fontsize',18)
% --------------------------------
axis off
