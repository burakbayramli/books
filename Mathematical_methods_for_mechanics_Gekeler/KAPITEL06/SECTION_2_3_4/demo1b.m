function demo1b
% Calculation for Kepler's 2-th law
% as DEMO1A.M but axes a and b specified
% INPUT G: corresponds to gravitational acceleration
%       M: Mass of central body in focal point fixed
%       m: Mass of mass point P
%       X: Position vector of P; V velocity vector in P
%       START in PERIHEL !!!!!
% OUTPUT: Trajectory in polar coordinates
% also diverse sections    
clc
% -- Parameter --------------
gamma = 1; M = 1; m = 1; alfa = 1;
% NUR ALFA  = 1 -------
%---------------------
a = 1; b = 1/2;
e = sqrt(a^2 - b^2); k = 1;
E = k/(2*a); D = b*sqrt(2*E); T = 2*pi*a*b/D;
Parmeter = [gamma,M,m,alfa,a,b,D,T];
% ---------------------------
phi_0 = 0; r0 = a + e; DPHI = D/r0^2; 
X_polar   = [0;a+e];     % = (phi_0,r_0)
V_polar   = [DPHI;0];   % = (phi_0_dot, r_0_dot)
X_start = [X_polar;V_polar];
options = odeset('Reltol',1E-5);
%%% Ellipse physikalisch %%%%%%%%%%%%%%%%%%%%%
TT = linspace(0,T,100);
[TEND,Y]  = ode45(@bsp01,TT,X_start,options,Parmeter);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Zeitdiff = 10; % Nummern in TT
%%% 1. Sektor %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
TA = 10; % Anfang, Nr. in TT
X_start = Y(TA,:)';
ZEIT1 = linspace(TT(TA),TT(TA+Zeitdiff),30);
[T,Y1] = ode45(@bsp01,ZEIT1,X_start,options,Parmeter);
%%% 2. Sektor %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
TA = 42; % Anfang, Nr. in TT
X_start = Y(TA,:)';
ZEIT2 = linspace(TT(TA),TT(TA+Zeitdiff),30);
[T,Y2] = ode45(@bsp01,ZEIT2,X_start,options,Parmeter);
%%% 3. Sektor %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TA = 56; % Anfang, Nr. in TT
X_start = Y(TA,:)';
ZEIT3 = linspace(TT(TA),TT(TA+Zeitdiff),30);
[T,Y3] = ode45(@bsp01,ZEIT3,X_start,options,Parmeter);
save daten1 TEND Y Parmeter Y1 Y2 Y3
fig0604
