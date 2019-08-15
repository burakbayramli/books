function test02
% EULER-LAGRANGE equations for EULER angles
% OUTPUT: trajectory in EULER angles
clc
format compact
done      = 0;
K         = [1 2 3 5 6 7 8 9];
%while ~done
%   nr   = input(' Beispiel Nr. (1/2/3/5/6/7/8/9) ');
%   done = ismember(nr,K);
%end;
%warning off MATLAB:ode23t:IntegrationTolNotMet
%options = odeset('RelTol',1E-3,'MaxStep',0.01,'Mass','bsp05b', ...
% 'massSingular','maybe','Initialslope',X0P,'MStateDependence','strong');
options = odeset('RelTol',1E-3,'MaxStep',0.01,'Mass','bsp05b', ...
'massSingular','maybe','MStateDependence','strong');

nr = 5;
switch nr
case 5 % FALL 5
   T1 = 1; T3 = 2; m = 1; gl = 0.9289/2;
   THETA1 = pi/4; THETA2 = pi/2;
   Parmeter = [T1, T3, m, gl];
   X0      = [0;pi/2;0;1;0;1];
   X0(3) = 0; % = PSI(0)
   %X0P     = bsp05c(0,X0,Parmeter);
   T_END   = 4
   
case 8 % FALL 8
   T1 = 1; T3 = 2; m = 1; gl = 1/2;
   THETA1 = 0; THETA2 = pi/2;
   Parmeter = [T1, T3, m, gl];
   X0      = [0,pi/2,0,3,0,3/2]; X0 = X0';
   X0(3) = 100; % = PSI(0)
   %X0P     = bsp05c(0,X0,Parmeter);
   T_END   = 4
end
[T,Y]  = ode23t(@bsp05a,[0, T_END],X0,options,Parmeter);
save daten T Y Parmeter THETA1 THETA2
bild01
