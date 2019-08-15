function demo1
% Computes trajectories in central field
% with numerical solution of a differential system
% INPUT G: corresponds to gravitational constant
%       M Mass of central body in focus FIXED
%       m Mass of mass point P
%       X position vector of P; V velocity vector of P
% OUTPUT: trajectory in cartesian coordinates
clc
format compact
% -- Parameter --------------
G = 1; M = 1; m = 1;
alfa = 1; % U(r) = - k/r^alfa;
% ---------------------------
Parmeter = [G,M,m,alfa];
X = [1; 1]; V = [-3;1];
V = V/norm(V);
V = 2*V/5;
X = [X;V];
options = odeset('Reltol',1E-5);
[T,Y] = ode45(@bsp01,[0 20],X,options,Parmeter);
save daten1 T X Y Parmeter
bild01
