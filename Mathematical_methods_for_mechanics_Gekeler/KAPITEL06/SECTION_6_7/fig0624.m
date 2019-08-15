function fig0624
% Figure 6.24, Spherical coordinates
% Surface with degrees of longitude and latitude
% Degree of longitude positive to east and negative to West
% Greenwich is PHI = 0, - PI <= PHI <= PI
% Degree of latitude positive to North and negative to South
% Aequator is THETA = 0, -PI/2 <= THETA <= PI/2

% -- Punkte  mit Laengen- und Breitengrad ----------
PHI = pi/3;  %L"angengrade im Uhrzeigersinn!
THETA = pi/5;
% --------------------------------------------------
clc, clf
az = 100; el = 30;  % gut
phi   = linspace(0,2*pi,50);
theta = linspace(-pi/2,pi/2,50);
% --------------------------------
h     = axes('Position',[0 0 1 1]);
% -- ACHSEN -----------------------
X1 = [-2.4, 2.4]; Y1 = [0 0]; Z1 = [0 0]; % X-Achse
X2 = [0 0]; Y2 = [-1.4 1.4]; Z2 = [0 0];  % Y-Achse
X3 = [0 0]; Y3 = [0 0]; Z3 = [-1.5 1.5];  % Z-Achse
plot3(X1,Y1,Z1,'k--','linewidth',2), hold on
plot3(X2,Y2,Z2,'k--','linewidth',2), hold on
plot3(X3,Y3,Z3,'k--','linewidth',2), hold on
view([az,el])
% -- Grosskreis in Cameraposition ------------
DD = huelle(az,el);
plot3(DD(1,:),DD(2,:),DD(3,:),'k','LineWidth',1.5), hold on

% Grosskreis in (y,z) - Ebene ----------
X = 1*cos(theta); Y = 0*cos(theta);Z = sin(theta);
plot3(X,Y,Z,'b','LineWidth',1.5), hold on
X = -1*cos(theta); Y = 0*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'b','LineWidth',1.5), hold on

% -- Grosskreis in (x,z)-Ebene ----------
X = 0*cos(theta); Y = -1*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'b','LineWidth',1.5), hold on
X = 0*cos(theta); Y = 1*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'b','LineWidth',1.5), hold on

% -- Punkt in Kugelkoordinaten -------------------
A = [cos(PHI)*cos(THETA);
     sin(PHI)*cos(THETA);
     sin(THETA)];
plot3(A(1),A(2),A(3),'o','MarkerSize',6), hold on

% -- Breitenkreis von Punkt A ------------------
X = cos(phi)*cos(THETA); Y = sin(phi)*cos(THETA);
Z = sin(THETA)*ones(length(phi),1);
plot3(X,Y,Z,'r'), hold on
% -- Laengen-Halbkreis von Punkt A --------
X = cos(PHI(1))*cos(theta); Y = sin(PHI(1))*cos(theta);
Z = sin(theta);
plot3(X,Y,Z,'g'), hold on
% ------- Achsenkreuz ----------------------
XX = [cos(PHI)*cos(THETA); sin(PHI)*cos(THETA); sin(THETA)];
YY = [-sin(PHI); cos(PHI); 0];
ZZ = [-cos(PHI)*sin(THETA);-sin(PHI)*sin(THETA); cos(THETA)];
XX = XX/norm(XX); YY = YY/norm(YY); ZZ = ZZ/norm(ZZ);
EX = [A, A+XX]; EY = [A, A+YY]; EZ = [A,A+ZZ];
h = 0.1; r = 0.02;
plot3(EX(1,:),EX(2,:),EX(3,:),'k','linewidth',2), hold on
plot3(EY(1,:),EY(2,:),EY(3,:),'k','linewidth',2), hold on
plot3(EZ(1,:),EZ(2,:),EZ(3,:),'k','linewidth',2), hold on
pfeil3(EX(1,:),EX(2,:),EX(3,:),h,r), hold on
pfeil3(EY(1,:),EY(2,:),EY(3,:),h,r), hold on
pfeil3(EZ(1,:),EZ(2,:),EZ(3,:),h,r), hold on

text(2.6,0,0,'e_1','fontsize',18)
text(0,1.5,0,'e_2','fontsize',18)
text(0,-0.3,1.4,'e_3','fontsize',18)
text(-0.3,1.2,0.8,'g_1','fontsize',18)
text(0,0.2,1.4,'g_2','fontsize',18)
text(-0.3,1.2,0.4,'g_3','fontsize',18)

grid on
%out = get(gca,'Cameraupvector')
axis equal tight
xlabel('x-Achse')
ylabel('y-Achse')
% --------------------------------
axis off
