function demo2
% Calculation for Figure 6.5
% Demo fuer Apsidenwinkel im Zentralfeld
% INPUT G: corresponds to gravitational acceleration
%       M: Mass of central body in focal point fixed
%       m: Mass of mass point P
%       X: Position vector of P; V velocity vector in P
% OUTPUT: Trajectory in polar coordinates

clc, format compact, format short
% -- Parameter ----------
G = 1; M = 1; m = 1;
alfa = 1.2 % U(r) = - k/r^alfa; % Potential
% -----------------------
Parmeter = [G,M,m,alfa];
% -- Anfangswerte -------
X  = [0;1.5;0.3;0]; %------
% -----------------------
k  = G*m*M;
r0 = X(2); dphi = X(3);
L  = m*r0*r0*dphi      % Drehimpuls (konstant!)
E  = 0.5*m*r0*r0*dphi*dphi - G*M*m/r0^alfa
a  = abs(k/(2*E))
V  = - k/X(2) + L*L/(2*m*X(2)*X(2));
options = odeset('Reltol',1E-5);
[T,Y]   = ode45(@bsp01,[0 33],X,options,Parmeter);
save daten3 T X Y
fig0605
