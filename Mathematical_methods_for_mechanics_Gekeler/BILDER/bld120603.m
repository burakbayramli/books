function bld060703
% BILD : Hypozykloiden
clc, clf
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
alpha  = 0;      %Startwert auf Rollkreis
alpha1 = pi/4;   % spezieller Winkel
cc = 0.6; d = 0.2;
% -- x-Achse ------------------
X = [- 1.3*R, 1.3*R]; Y = [0, 0];
arrow(X,Y,cc,d,'k',1)
% -- y-Achse ----------------------
X = [0, 0]; Y = [-1.3*R, 1.3*R];
arrow(X,Y,cc,d,'k',1)

PHI = linspace(0,2*pi,100);
PSI = (R - r)*PHI/r;
% -- Kreise mit Radius R -----------
X = R*cos(PHI); Y = R*sin(PHI);
plot(X,Y,'k','LineWidth',2), hold on
X = [0, R*cos(3*pi/4)]; Y = [0, R*sin(3*pi/4)];
%arrow(X,Y,0.2, 0.1,'k',1);
hold on

% -- Kreis mit Radius R - r ----------
X = (R - r)*cos(PHI); Y = (R - r)*sin(PHI);
plot(X,Y,'k--','LineWidth',2), hold on
X = [0, (R-r)*cos(11*pi/8)]; Y = [0, (R-r)*sin(11*pi/8)];
%arrow(X,Y,0.2, 0.1,'k',1)
% -- erster Rollkreis ----------------
X = (R-r) + r*cos(PHI); Y = r*sin(PHI);
plot(X,Y,'k'), hold on

% zweiter Rollkreis -----------------
MX = (R-r)*cos(alpha1); MY = (R-r)*sin(alpha1);
X = MX + r*cos(PHI + alpha); Y = MY + r*sin(PHI + alpha);
plot(X,Y,'k'), hold on

% -- Hypozykloide ------------
X = (R-r)*cos(PHI) + c*cos(PSI + alpha);
Y = (R-r)*sin(PHI) - c*sin(PSI + alpha);
plot(X,Y,'k','LineWidth',2), hold on

% -- Zwei Punkte auf der Hypozykolide ----
% -- alpha = 0 -------------------------------
X1 = R-r+c*cos(alpha); Y1 = -c*sin(alpha);
% -- alpha = alpha1 ---------------------------
X2 = (R-r)*cos(alpha1) + c*cos((R-r)*alpha1/r + alpha);
Y2 = (R-r)*sin(alpha1) - c*sin((R-r)*alpha1/r + alpha);
plot(X2,Y2,'.','Markersize',18), hold on
% -- Die Strecke von M_1 nach P ----------
MX = (R-r)*cos(alpha1); MY = (R-r)*sin(alpha1);
X6 = [MX, X2]; Y6 = [MY, Y2];
plot(X6,Y6,'k'), hold on
RR = 0.2;
circle(X1,Y1,RR,'w')
circle(X2,Y2,RR,'w')

% Markierung von Punkten und Strecken ---
MX1 = (R-r)*cos(alpha1);
MY1 = (R-r)*sin(alpha1);
X5 = [0 MX1]; Y5 = [0 MY1];
plot(X5,Y5,'k'), hold on
circle(0,0,RR,'w')
circle(R,0,RR,'w')
circle(MX1,MY1,RR,'w')
switch flag
case 1 % Bild 1
   text(1,6.8,'r = 3, c = 1','fontsize',24)
case 2 % Bild 2
   text(1,6.8,'r = 3, c = 3','fontsize',24)
case 3 % Bild 3
   text(1,6.8,'r = 3, c = 4','fontsize',24)
case 4 % Bild 4
   text(1,6.8,'r = 2, c = 1','fontsize',24)
case 5 % Bild 5
   text(1,6.8,'r = 2, c = 2','fontsize',24)
case 6 % Bild 6
   text(1,6.8,'r = 2, c = 4','fontsize',24)
end
grid on
axis equal tight
axis off
