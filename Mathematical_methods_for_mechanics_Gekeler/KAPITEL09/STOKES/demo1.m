function demo1
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: LID DRIVEN CAVITY
% Linear Stokes problem, Taylor-Hood elements
% X,Y : coordinates
% U,V : velocity in x- and y-direction
% P   : Pressure
% p,e,t: nodes,geometric boundary data, triangles
% p1,t1: data of intermediate nodes and their indices in triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]
% FF1   : File for first mesh (not used in MATLAB TOOLBOX)
% FF2   : File for geometry in MATLAB format 
% FF3   : File for boundary conditions and loads  
% OPTION_MESH = 1/2 Mesh without/with MATLAB TOOLBOX
% OPTION_P = 1: Specify one node value for pressure 
% OPTION_P = 2: int p dxdy = 0 for additional boundary condition 
% OPTION_P = 2: slower but better result for pressure
% OPTION_P = 3: as OPTION_P = 2 but with augmented quadratic matrix

clear, clc, format short, format compact
% Example: 
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h_1';
% Segment nrs. of outer boundary = [1,2,3,4]
% -- Parameters ---------------
nu      = 0.001;     % coeff. of viscosity
epsilon = 0;         % Penalty Term, epsilon = 0.1 for stabilization
Parmeter = [nu,epsilon];
OPTION_MESH = 1; REFINE = 3; % Number of uniform mesh refinements
%OPTION_MESH = 2; REFINE = 4; % Number of uniform mesh refinements
OPTION_P = 2;
% -----------------------------
[p,e,t,p1,t1] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
% -- Load boundary values and loads ----
[RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1);
N = size(p,2) + size(p1,2); % total node number
switch OPTION_P
case 1 % fix a special value of pressure
   RD = [RDU(1,:), RDV(1,:) + N,RDP(1)+2*N;
         RDU(2,:), RDV(2,:)    ,RDP(2)];
   [U,V,P] = stokes1a(p,p1,t,t1,FU,FV,RD,Parmeter);
case 2, % alternatively without specifying a pressure value
   RD = [RDU(1,:), RDV(1,:) + N;
         RDU(2,:), RDV(2,:)];
   [U,V,P] = stokes1b(p,p1,t,t1,FU,FV,RD,Parmeter);
end
save daten1a p e t p1 t1 
save daten1b U V P 

disp(' bild01 is called! Call also bild06! ')
bild01
 