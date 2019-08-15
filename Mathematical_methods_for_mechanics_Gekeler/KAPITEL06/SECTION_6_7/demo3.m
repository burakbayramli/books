function demo3
% Demo for top
% Calculates EULER angles PHI and THETA from the data of
% DEMO2.M by differential system for THETA,D_THETA,PHI;
% OUTPUT: trajectory of axis of top on surface of ball
% trajectory does not depend on PSI and T3
% We have here D_THETA = 0 at the limit angles THETA1 and THETA2.
% Therefore initial conditions:
% THETA = THETA1 or THETA = THETA2, D_THETA = 0, PHI = 0;
%
clc, format compact
done      = 0;
K         = [1 2 3 5 6 7 8 9];
while ~done
   nr   = input(' Beispiel Nr. (1/2/3/5/6/7/8/9) ');
   done = ismember(nr,K);
end;
options = odeset('Reltol',1E-5,'Maxstep',0.01);
%warning off MATLAB:ode23:IntegrationTolNotMet
switch nr
case 1
   T1 = 1; T3 = 2; m = 1; THETA1 = pi/6, THETA2 = pi/2
   d3 = 3; D3 = 2; beta = 2.9667; gl = beta/2;
   X0 = [pi/6;0;0]; T_END = 10;
   Parmeter = [T1,T3,m,gl,d3,D3];
case 2
   T1 = 1; T3 = 2; m = 1; THETA1 = pi/2, THETA2 = 3*pi/4;
   d3 = -3; D3 = 1; beta = 2.1421; gl = beta/2;
   X0 = [pi/2;0;0]; T_END = 10;
   Parmeter = [T1,T3,m,gl,d3,D3];
case 3
    T1 = 1; T3 = 2; m = 1; THETA1 = pi/6, THETA2 = pi/2;
    d3 = sqrt(3); D3 = 2; beta = 3.4641; gl = beta/2;
    X0 = [pi/6;0;0]; T_END = 10;
    Parmeter = [T1,T3,m,gl,d3,D3];
case 5
   T1 = 1; T3 = 2; m = 1; THETA1 = pi/4, THETA2 = pi/2;
   d3 = 1; D3 = 2; beta = 0.9289; gl = beta/2;
   X0 = [pi/4;0;0]; T_END = 10;
   Parmeter = [T1,T3,m,gl,d3,D3];
case 6
   T1 = 1; T3 = 2; m = 1; THETA1 = pi/4, THETA2 = pi/4;
   d3 = 1.2; D3 = 1; beta = 0.5973; gl = beta/2;
   X0 = [pi/4;0;0]; T_END = 10;
   Parmeter = [T1,T3,m,gl,d3,D3];
case 7
   T1 = 1; T3 = 2; m = 1; THETA1 = pi/2, THETA2 = pi;
   d3 = -1; D3 = 1; beta = 1; gl = beta/2;
   X0 = [pi/2;0;0]; T_END = 10;
   Parmeter = [T1,T3,m,gl,d3,D3];
case 8
   T1 = 1; T3 = 2; m = 1; THETA1 = 0, THETA2 = pi/2;
   d3 = 3; D3 = 3; beta = 1; gl = beta/2;
   X0 = [pi/2;0;0]; T_END = 10;
   Parmeter = [T1,T3,m,gl,d3,D3];
case 9
   T1 = 1; T3 = 2; m = 1; THETA1 = 0, THETA2 = pi/2;
   d3 = 1; D3 = 1; beta = 1; gl = beta/2;
   X0 = [pi/2;0;0]; T_END = 10;
  X0 = [pi/4;0;0]; T_END = 10;
   Parmeter = [T1,T3,m,gl,d3,D3];
end
[T,Y]   = ode23(@bsp03,[0, T_END],X0,options,Parmeter);
save daten T Y Parmeter THETA1 THETA2 d3 D3
fig0629
