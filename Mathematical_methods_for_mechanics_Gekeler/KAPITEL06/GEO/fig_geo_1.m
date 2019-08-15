function fig_geo_1
% Spur der Kreiselachse auf der Kugeloberflaeche
% PRAE = Praezessionswinkel
% NUT  = Nutationswinkel

% -- Punkte  ---------
PRAE = 3*pi/10;
NUT = 0;
PHI = -pi/3;  %L"angengrade im Uhrzeigersinn!
THETA = pi/2 - PRAE;
% -----------------------------------------------------------
% --------------------------------------------------
clc, clf
az = 100; el = 30;  % gut
phi   = linspace(0,2*pi,50);
theta = linspace(-pi/2,pi/2,50);
% --------------------------------
h     = axes('Position',[0 0 1 1]);
% -- Grosskreis in Cameraposition ------------
DD = huelle(az,el);
plot3(DD(1,:),DD(2,:),DD(3,:),'k','LineWidth',1.5), hold on
view([az,el])
% Grosskreis in (y,z) - Ebene ----------
X = 1*cos(theta); Y = 0*cos(theta);Z = sin(theta);
plot3(X,Y,Z,'LineWidth',1.5), hold on
X = -1*cos(theta); Y = 0*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'LineWidth',1.5), hold on
% -- Grosskreis in (x,yz)-Ebene ----------
X = 0*cos(theta); Y = -1*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'LineWidth',1.5), hold on
X = 0*cos(theta); Y = 1*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'LineWidth',1.5), hold on
% -- Praezessionsachse -------------------
A = [cos(PHI)*cos(THETA); sin(PHI)*cos(THETA);
     sin(THETA)];
plot3(A(1),A(2),A(3),'.','MarkerSize',6), hold on
% -- Kreiselachse -------------------------
THETA1 = THETA+pi/10;
B = [cos(PHI)*cos(THETA1); sin(PHI)*cos(THETA1);
     sin(THETA1)];
plot3(B(1),B(2),B(3),'.','MarkerSize',6), hold on
PSI = pi/4;
DD = drehmatrix(PSI,A);
C = DD*B;
plot3(C(1),C(2),C(3),'o','MarkerSize',6), hold on

% -- Spur der Kreiselachse ------------------
NN = 100; MM = 4000;
DPHI = 2*pi/NN; DPSI = 2*pi/MM;
SPUR = B;
for I = 1:40
    B = [cos(PHI+I*DPHI)*cos(THETA); sin(PHI+I*DPHI)*cos(THETA);
        sin(THETA)];
    DD = drehmatrix(I*DPSI,B);
    D = DD*C;
    SPUR = [SPUR,B];
    C = D;
end
plot3(SPUR(1,:),SPUR(2,:),SPUR(3,:),'g'), hold on

% -- Breitenkreise des Nutationsringes-------------
THETA1 = THETA+pi/10;
X = cos(phi)*cos(THETA1); Y = sin(phi)*cos(THETA1);
Z = sin(THETA1)*ones(length(phi),1);
plot3(X,Y,Z,'r'), hold on
THETA1 = THETA-pi/10;
X = cos(phi)*cos(THETA1); Y = sin(phi)*cos(THETA1);
Z = sin(THETA1)*ones(length(phi),1);
plot3(X,Y,Z,'r'), hold on

lflag = 0;
if lflag == 1
   % -- Laengen-Halbkreis von Punkt A --------
   X = cos(PHI(1))*cos(theta); Y = sin(PHI(1))*cos(theta);
   Z = sin(theta);
   plot3(X,Y,Z,'g'), hold on
end
grid on
%out = get(gca,'Cameraupvector')
% -- ACHSEN -----------------------
%X1 = [-2.4, 2.4]; Y1 = [0 0]; Z1 = [0 0]; % X-Achse
%X2 = [0 0]; Y2 = [-1.4 1.4]; Z2 = [0 0];  % Y-Achse
%X3 = [0 0]; Y3 = [0 0]; Z3 = [-1.5 1.5];  % Z-Achse
%plot3(X1,Y1,Z1,'k--','linewidth',2), hold on
%plot3(X2,Y2,Z2,'k--','linewidth',2), hold on
%plot3(X3,Y3,Z3,'k--','linewidth',2), hold on
%view([az,el])

axis equal tight
xlabel('x-Achse')
ylabel('y-Achse')
% --------------------------------
%axis off
