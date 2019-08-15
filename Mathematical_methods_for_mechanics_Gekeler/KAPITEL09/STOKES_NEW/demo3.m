function demo3
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: FLOW PAST HALF-CYLINDER
% Linear, Taylor-Hood elements
% X,Y : coordinates
% U,V : velocity in x- and y-direction
% P   : Pressure
% p,e,t: nodes,edges,triangles
% p1,t1: data of intermediate nodes and their indices in triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]
% FF1  : File for first mesh (not used in MATLAB TOOLBOX)
% FF2  : File for geometry in MATLAB format 
% FF3  : File for boundary conditions and loads   
% OPTION_MESH = 1/2 Mesh without/with MATLAB TOOLBOX
% OPTION_P = 1: Specify one node value for pressure 
% OPTION_P = 2: int p dxdy = 0 for additional boundary condition 
% U0: velocity constant at inflow
 
clear, clc, format short, format compact
Example = 2;
   switch Example
   case 1
      FF1 = 'bsp03_1'; FF2 = 'bsp03g_1'; FF3 = 'bsp03h_1'; U0 = 1;
      % Segment nrs. for boundary = [1,2,3,7,8,9,5,6]
   case 2 % modified geometry
      FF1 = 'bsp03_2'; FF2 = 'bsp03g_2'; FF3 = 'bsp03h_2'; U0 = 1; 
      % Segment nrs. for boundary = [1,2,3,7,8,9,5,6]
   end
% -- Parameters ---------------
NU      = 0.01;  % coeff. of viscosity
EPSILON = 0;     % Penalty Term
Parmeter = [NU,EPSILON];
%OPTION_MESH = 2; REFINE = 3;  % Number of uniform mesh refinements
OPTION_MESH = 1; REFINE = 3;  % Number of uniform mesh refinements
OPTION_P = 2;
% -----------------------------
%Start = 100; while ~ismember(Start,[0,1])
%   Start = input(' New start or Restart? (1/0) ');
%end
Start = 1;  
if Start == 1  
   [p,e,t] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   [p1,e,t1]  = mesh06_t(p,e,t); % e is enlarged by a further row 8
   %bild00(p,e,t), pause
   save daten3a p e t p1 t1 U0 FF3
end
load daten3a p e t p1 t1 U0
% -- Load boundary values and loads ----
[RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1,U0);
N = size(p,2) + size(p1,2);
switch OPTION_P
case 1
   RD = [RDU(1,:), RDV(1,:) + N,RDP(1)+2*N;
         RDU(2,:), RDV(2,:)    ,RDP(2)];
   [U,V,P] = stokes1a(p,p1,t,t1,FU,FV,RD,Parmeter);
case 2, % alternatively without specifying a pressure value
   RD = [RDU(1,:), RDV(1,:) + N;
         RDU(2,:), RDV(2,:)];
   [U,V,P] = stokes1b(p,p1,t,t1,FU,FV,RD,Parmeter);
end
save daten3b U V P 
disp(' Call bild03! ')
