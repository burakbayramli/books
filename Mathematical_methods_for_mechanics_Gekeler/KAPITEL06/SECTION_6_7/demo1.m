function demo1
% Calculates EULER angles for top directly from
% EULER-LAGRANGE equations
% OUTPUT: TRajectory of top's axis
% THETA(0) must be nonzero!!

clc, format compact, format short
done      = 0;
K         = [1 2];
%while ~done
%   nr   = input(' Beispiel Nr. (1/2) ');
%   done = ismember(nr,K);
%end;
options = odeset('Reltol',1E-7,'Maxstep',0.001);
%warning off MATLAB:ode23:IntegrationTolNotMet
nr = 1;
switch nr
case 1
   T1 = 1; T3 = 2; m = 1; gl = 2.9667/2; tol = 1.0E-3;
   d3 = 3; D3 = 2; THETA1 = pi/6; THETA2 = pi/2;
   dphi = (d3 - D3*cos(THETA1))/(T1*sin(THETA1)^2);
   dpsi = D3/T3 - dphi*cos(THETA1);
   X0 = [0;THETA1;0;dphi;0;dpsi];
   Parmeter = [T1, T3, m, gl, tol];
   T_END = 6
end
[T,Y]   = ode23(@bsp01,[0, T_END],X0,options,Parmeter);
save daten T Y Parmeter THETA1 THETA2
bild01
