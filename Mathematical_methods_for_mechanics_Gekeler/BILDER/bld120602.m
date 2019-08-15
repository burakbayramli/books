function bld060702
% BILD : Epizykloiden
clc,clf
R = 6;
flag = 100;
while ~ismember(flag,[1,2,3,4,5,6])
   flag = input('Welches Bild? (1/2/3/4/5/6)');
end
%flag = 6
switch flag
case 1; r = 3; c = 1;
case 2; r = 3; c = 3;
case 3; r = 3; c = 4;
case 4; r = 2; c = 1;
case 5; r = 2; c = 2;
case 6; r = 2; c = 4;
end
cc = 1; d = 0.35;
psi = pi;     %Startwert auf Rollkreis
phi = pi/4;   % spezieller Winkel
% -- x-Achse ------------------
r0 = 3;
X = [-(R+2.5*r0), R+2.5*r0]; Y = [0, 0];
arrow(X,Y,cc,d,'k',1)
% -- y-Achse ----------------------
X = [0, 0]; Y = [0, R + 2.5*r0];
arrow(X,Y,cc,d,'k',1)
PHI = linspace(0,2*pi,100);
PSI = (R + r)*PHI/r;
% -- Kreise mit Radius R -----------
X = R*cos(PHI); Y = R*sin(PHI);
plot(X,Y,'k','LineWidth',2), hold on
X = [0, R*cos(3*pi/4)]; Y = [0, R*sin(3*pi/4)];
%arrow(X,Y,cc,d,'k',1)
% -- Kreis mit Radius R + r ----------
X = (R + r)*cos(PHI); Y = (R + r)*sin(PHI);
plot(X,Y,'k--','LineWidth',2), hold on
X = [0, (R+r)*cos(11*pi/8)]; Y = [0, (R+r)*sin(11*pi/8)];
%arrow(X,Y,cc,d,'k',1)
% -- erster Rollkreis ----------------
X = (R+r) + r*cos(PHI); Y = r*sin(PHI);
plot(X,Y,'k'),hold on
% zweiter Rollkreis -----------------
MX = (R+r)*cos(phi); MY = (R+r)*sin(phi);
X = MX + r*cos(PHI + psi); Y = MY + r*sin(PHI + psi);
plot(X,Y,'k'), hold on
% -- Epizykloide ------------
X = (R+r)*cos(PHI) - c*cos(PSI + psi);
Y = (R+r)*sin(PHI) - c*sin(PSI + psi);
plot(X,Y,'k','LineWidth',2), hold on
% -- Zwei Punkte auf der Epizykolide ----
% -- phi1 = 0 -------------------------------
X1 = R+r-c*cos(psi); Y1 = -c*sin(psi);
% -- phi2 = phi ---------------------------
X2 = (R+r)*cos(phi) - c*cos((R+r)*phi/r + psi);
Y2 = (R+r)*sin(phi) - c*sin((R+r)*phi/r + psi);
% -- Die Strecke von M_1 nach P ----------
MX = (R+r)*cos(phi); MY = (R+r)*sin(phi);
X6 = [MX, X2]; Y6 = [MY, Y2];
plot(X6,Y6,'k'), hold on
RR = 0.3;
circle(X1,Y1,RR,'w')
circle(X2,Y2,RR,'w')

% Markierung von Punkten und Strecken ---
MX1 = (R+r)*cos(phi); MY1 = (R+r)*sin(phi);
X5 = [0 MX1]; Y5 = [0 MY1];
plot(X5,Y5,'k'), hold on
circle(0,0,RR,'w')
circle(R,0,RR,'w')
circle(MX1,MY1,RR,'w')
switch flag
case 1 % Bild 1
   text(1,12,'r = 3, c = 1','fontsize',24)
case 2 % Bild 2
   text(1,12,'r = 3, c = 3','fontsize',24)
case 3 % Bild 3
   text(1,12,'r = 3, c = 4','fontsize',24)
case 4 % Bild 4
   text(1,12,'r = 2, c = 1','fontsize',24)
case 5 % Bild 5
   text(1,12,'r = 2, c = 2','fontsize',24)
case 6 % Bild 6
   text(1,12,'r = 2, c = 4','fontsize',24)
end

grid on
axis equal tight
axis off
