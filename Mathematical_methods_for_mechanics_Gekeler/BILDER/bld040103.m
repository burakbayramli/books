% Zykloiden

clf
r = 2; q = 2;
psi = 0;     %Startwert auf Rollkreis
psi = pi;     %Startwert auf Rollkreis
phi = pi/4;   % spezieller Winkel

% -- x-Achse ------------------
X = [-5, 17];
Y = [0, 0];
arrow(X,Y,0.4, 0.2,'k',1);
hold on
% -- y-Achse ----------------------
X = [0, 0];
Y = [0.5, -5];
arrow(X,Y,0.4, 0.2,'k',1);

PHI = linspace(-3*pi/4,11*pi/4,100);
PSI = linspace(0,2*pi,100);
% -- Zykloide ------------
X = r*PHI - q*sin(PHI);
Y = -(r - q*cos(PHI));
plot(X,Y,'LineWidth',2), hold on

% -- erster Rollkreis ----------------
phi = 5*pi/8;
X   = r*phi + r*cos(PSI);
Y   = -(r + r*sin(PSI));
plot(X,Y), hold on
X1 = r*phi;
Y1 = -r;
Radius = 0.15;
X2 = r*phi - q*sin(phi);
Y2 = -(r - q*cos(phi));
X3 = r*phi;
Y3 = 0;
X4 = [X1, X2];
Y4 = [Y1, Y2];
plot(X4,Y4), hold on
X5 = [X1, X3];
Y5 = [Y1, Y3];
plot(X5,Y5), hold on
circle(X1,Y1,Radius,'w')
circle(X2,Y2,Radius,'w')
circle(X3,Y3,Radius,'w')

% zweiter Rollkreis -----------------
phi = 2*pi - 3*pi/8;
X   = r*phi + r*cos(PSI);
Y   = -(r + r*sin(PSI));
plot(X,Y), hold on
X1 = r*phi;
Y1 = -r;
X2 = r*phi - q*sin(phi);
Y2 = -(r - q*cos(phi));

X3 = r*phi;
Y3 = 0;
X4 = [X1, X2];
Y4 = [Y1, Y2];
plot(X4,Y4), hold on
X5 = [X1, X3];
Y5 = [Y1, Y3];
plot(X5,Y5), hold on
circle(X1,Y1,Radius,'w')
circle(X2,Y2,Radius,'w')
circle(X3,Y3,Radius,'w')

% -- Rahmen
RX = [-5.5, 17.5, 17.5, -5.5,-5.5];
RY = [-5.5, -5.5,0.7,0.7,-5.5];
plot(RX,RY,'k','linewidth',2)
% -- Beschriftung   ---------------
text(16.3,-0.6,'x','Fontsize',22)
text(-0.9,-4.5,'y','Fontsize',22)
axis equal tight
axis off
