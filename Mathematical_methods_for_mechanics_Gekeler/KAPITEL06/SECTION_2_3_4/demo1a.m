function demo1a
% Calculation for Kepler's 2-th law
% INPUT G: corresponds to gravitational acceleration
%       M: Mass of central body in focal point fixed
%       m: Mass of mass point P
%       X: Position vector of P; V velocity vector in P
%       START in PERIHEL !!!!!
% OUTPUT: Trajectory in polar coordinates

format short, format compact
% -- Parameter --------------
gamma = 1; M = 1; m = 1; alfa = 1;
% NUR ALFA  = 1 -------
%---------------------
Parmeter = [gamma,M,m,alfa];
DPHI    = 2/5;
X       = [1; 0]; V = [0;1];
V       = DPHI*V;  % Ellipse
X_polar   = [0;1];     % = (phi_0,r_0)
V_polar   = [DPHI;0]   % = (phi_0_dot, r_0_dot)

X_start = [X_polar;V_polar];
options = odeset('Reltol',1E-5);
Zeitdiff = 0.3
[T,Y]  = ode45(@bsp01,[0, -0.1],X_start,options,Parmeter);
X1     = Y(size(Y,1),:);
[T,Y1] = ode45(@bsp01,[-0.1, -0.1+Zeitdiff],X1,options,Parmeter);
X2     = Y1(size(Y1,1),:);
[T,Y]  = ode45(@bsp01,[0, 1.2],X_start,options,Parmeter);
X3     = Y(size(Y,1),:);
[T,Y2] = ode45(@bsp01,[1.2, 1.2+Zeitdiff],X3,options,Parmeter);
X4     = Y2(size(Y2,1),:);
XX     = [X1',X2',X3',X4'];
save daten1 T X V XX Y1 Y2 Parmeter
bild01
