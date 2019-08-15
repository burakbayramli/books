% Bild005.m
clf
az = -37.5;
%az = 90;
el = 30; %gut
%el = 0;
az = 100;
k     = 5;
n     = 2^k-1;
l = 4;
m = 2^l-1;
phi   = pi*(0:2:2*n)/n;
theta =(pi/2)*(-m:2:m)'/m;
psi   = pi*(-m:2:0)/m;

% --------------------------------
h = axes('Position',[0 0 1 1]);
%axes('Position',[0 0 0.8 1])
% ACHSEN -------------------------
X = [-1.5 1.5];
Y = [0 0];
Z = [0 0];
view([az,el])
plot3(X,Y,Z,'r')
hold on

X = [0 0];
Y = [-1.5 1.5];
Z = [0 0];
view([az,el])
plot3(X,Y,Z,'g')
hold on
X = [0 0];
Y = [0 0];
Z = [-1.5 1.5];
view([az,el])
plot3(X,Y,Z,'k')
hold on

% Grosskreis in Cameraposition
DD = huelle(az,el);
view([az,el])
plot3(DD(1,:),DD(2,:),DD(3,:),'k','LineWidth',1.5);
hold on

% Grosskreise in (x,z)- und (y,z)- Ebene ----------
X = 1*cos(theta);
Y = 0*cos(theta);
Z = sin(theta);
view([az,el])
plot3(X,Y,Z,'LineWidth',1.5);
hold on

X = -1*cos(theta);
Y = 0*cos(theta);
Z = sin(theta);
view([az,el])
plot3(X,Y,Z,'LineWidth',1.5);
hold on

X = 0*cos(theta);
Y = -1*cos(theta);
Z = sin(theta);
view([az,el])
plot3(X,Y,Z,'LineWidth',1.5);
hold on

X = 0*cos(theta);
Y = 1*cos(theta);
Z = sin(theta);
view([az,el])
plot3(X,Y,Z,'LineWidth',1.5);
hold on

% Breitengrade  -------------------------------
flag = 0;
M = length(theta);
if flag == 1
   for I = 1:M
      X = cos(phi)*cos(theta(I));
      Y = sin(phi)*cos(theta(I));
      Z = sin(theta(I))*ones(length(phi),1);
      view([az,el])
      plot3(X,Y,Z,'r');
      hold on
   end
   % Laengengrade ---------------------
   N = length(psi);
   for I = 1:N
      X = cos(psi(I))*cos(theta);
      Y = sin(psi(I))*cos(theta);
      Z = sin(theta);
      view([az,el])
      plot3(X,Y,Z,'b');
      hold on
   end
end

% Verbindungzweier Staedte ---------------------------------
% Laengengrade im Uhrzeigersinn vom Nordpol aus gesehen
% STUTTGART:
L1 = (9 + 12/60)*pi/180;  %9 Grad 12 Sec Ost Laenge
L2 = (48 + 47/60)*pi/180;  % 48 Grad 47 Sec Nord Breite
% RIO DE JANEIRO:
J1 = (360 - (43+ 17/60))*pi/180;  % 43 Grad 17 Sec West Laenge
J2 = -(22 + 53/60)*pi/180;        % 22 Grad 53 Sec Sued Breite
PHI   = [L1; J1];
THETA = [L2; J2];

A = [cos(PHI(1))*cos(THETA(1));  % Lissabon
     sin(PHI(1))*cos(THETA(1));
     sin(THETA(1))];

B = [cos(PHI(2))*cos(THETA(2));  % Rio
     sin(PHI(2))*cos(THETA(2));
     sin(THETA(2))];

% Stadt A mit Laengen- und Breitenkreis

% Stadt A
view([az,el])
plot3(A(1),A(2),A(3),'*','MarkerSize',6);
hold on

% Breitenkreis von A
X = cos(phi)*cos(THETA(1));
Y = sin(phi)*cos(THETA(1));
Z = sin(THETA(1))*ones(length(phi),1);
view([az,el])
plot3(X,Y,Z,'r');
hold on

%Laengen-Halbkreis von Stadt A
X = cos(PHI(1))*cos(theta);
Y = sin(PHI(1))*cos(theta);
Z = sin(theta);
view([az,el])
plot3(X,Y,Z,'b');
hold on


% Stadt B mit Laengen- und Breitenkreis
% Stadt B
view([az,el])
plot3(B(1),B(2),B(3),'o','MarkerSize',6);
hold on

% Breitenkreis von Stadt B
X = cos(phi)*cos(THETA(2));
Y = sin(phi)*cos(THETA(2));
Z = sin(THETA(2))*ones(length(phi),1);
view([az,el])
plot3(X,Y,Z,'r');
hold on

%L„ngen-Halbkreis von Stadt B
X = cos(PHI(2))*cos(theta);
Y = sin(PHI(2))*cos(theta);
Z = sin(theta);
view([az,el])
plot3(X,Y,Z,'b');
hold on

% Verbindung der Staedte ---------------------
DD = gkreis(PHI,THETA);
view([az,el])
plot3(DD(1,:),DD(2,:),DD(3,:),'k');
hold on
% ------------------------------------------
text(0,0,1.5,'N','fontsize',12);
text(0,0,-1.5,'S','fontsize',12);
text(0.5,-1,-0.8,'RIO','fontsize',12);
text(0.5,0,0.8,'STGT','fontsize',12);

axis equal
%grid on
%out = get(gca,'Cameraupvector')
%axis equal tight
xlabel('x-Achse')
ylabel('y-Achse')
zlabel('z-Achse')
% --------------------------------
%axis off
