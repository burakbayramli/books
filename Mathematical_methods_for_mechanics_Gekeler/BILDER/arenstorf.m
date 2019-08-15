% Berechnet ARENSTORF-Orbits mit Mondmasse MU = 0
clf
clear
m = 1; k = 2;
m = 4; k = 1;
a = (m/k)^(2/3);
EPS = 0.5;   %   0 < EPS < 1 Parameter
PERIODE = 2*pi*m;
PHI = linspace(0,6*PERIODE,400);
T = (m/k)*(PHI + EPS*sin(PHI));
J = find(T <= PERIODE);
PHI = PHI(J);
T = (m/k)*(PHI + EPS*sin(PHI));
W = a*(EPS + cos(PHI) + i*sqrt(1 - EPS*EPS)*sin(PHI));
Z = exp(-i*T).*W;
X = real(Z);
Y = imag(Z);
plot(X,Y), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PHI = linspace(0,6*PERIODE,50);
T = (m/k)*(PHI + EPS*sin(PHI));
J = find(T <= PERIODE);
PHI = PHI(J);
T = (m/k)*(PHI + EPS*sin(PHI));
W = a*(EPS + cos(PHI) + i*sqrt(1 - EPS*EPS)*sin(PHI));
Z = exp(-i*T).*W;
X = real(Z);
Y = imag(Z);
plot(X,Y,'.','markersize',6),hold on
circle(0,0,0.01,'w')
c = 0.1;
d = 0.03;
X = [-1,1];
Y = [0,0];
arrow(X,Y,c,d,'k',2)
X = [0,0];
Y = [-1,1];
arrow(X,Y,c,d,'k',2)
axis equal tight
