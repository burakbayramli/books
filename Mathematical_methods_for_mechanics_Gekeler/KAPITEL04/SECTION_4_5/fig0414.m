function fig0414
% Figure 4.14, Reentry problem, general situation

clc, clf
% -- Erdoberflaeche ----------------
a = 0.5; R = 20.9;
T = linspace(pi/2-a,pi/2+a,40);
X1 = R*cos(T); Y1 = - R + R*sin(T);
plot(X1,Y1,'linewidth',2), hold on
% -- Flugbahn ----------------------
R1 = 40; a1 = 0.8; a2 = 1;
T1 = linspace(3*pi/2-0.55,3*pi/2-0.1,40);
X2 = 12 + R1*cos(T1); Y2 = R1 + 1.5 + R1*sin(T1);
plot(X2,Y2,'k','linewidth',2), hold on
% -- Tangente an Erdoberflaeche
r = 0.2;
AA = 0.304;     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
T1 = 3*pi/2-AA;
X2 = 12 + R1*cos(T1); Y2 = R1 + 1.5 + R1*sin(T1);
X8 = [0,7]; Y8 = [Y2,Y2];
c = 0.5; d = 0.2;
arrow(X8,Y8,c,d,'k',2)
Y8 = [0,0];
arrow_6(X8,Y8,c,d,'k',1,2)
X9 = [X2,X2 - 7*sin(T1)]; Y9 = [Y2,Y2 + 7*cos(T1)];
% -- Flugrichtung -------------
arrow(X9,Y9,c,d,'k',2)
circle(X2,Y2,r,'w'), circle(0,0,r,'w')
% -- Flugwinkel --------------------
a = -0.3; b =  0.01; c = 5;
TT = linspace(a,b,10);
X7 = X2 + c*cos(TT); Y7 = Y2 + c*sin(TT);
plot(X7,Y7,'linewidth',2), hold on
X8 = [8,5.2]; Y8 = [2.6,2.6];
c = 0.4; d = 0.1;
arrow(X8,Y8,c,d,'k',1)
% -- Hoehe -------------------------
b = 0.3;
PHI = pi/2 + b;
X3 = R*cos(PHI); Y3 = - R + R*sin(PHI);
r = 0.2;
X4 = (R + 8.6)*cos(PHI); Y4 = - R +  (R + 8.6)*sin(PHI);
r = 0.2;
X5 = [X3,X4]; Y5 = [Y3,Y4];
plot(X5,Y5,'linewidth',2), hold on
circle(X3,Y3,r,'w'), circle(X4,Y4,r,'w')

text(-5,-1.2,'surface of earth','fontsize',22)
text(-4,6,'flight path','fontsize',22)
text(-7,3,'altitude','fontsize',22)
text(7,1,'v','fontsize',22)
text(8.5,2.5,'\gamma','fontsize',22)

axis equal
%grid on
%title('Beispiel 10','fontsize',18)
