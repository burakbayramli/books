function demo5
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: UNIT SQUARE
% Linear, Taylor-Hood elements
% With exact solution to compare
% X,Y : coordinates
% U,V : velocity in x- and y-direction
% P   : pressure
% p,e,t: nodes,edges,triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]
% p1,t1: data of intermediate nodes and their indices in triangles
% FF1  : File for first mesh (dropped in using MATLAB TOOLBOX)
% FF2  : File for geometry in MATLAB format 
% FF3  : File for boundary conditions
% OPTION_MESH = 1/2 Mesh without/with MATLAB TOOLBOX
% Data of exact solution:   
% (U,V) = (x^3,-3*x^2*y), P = (x^3 + y^3)
% Right side of linear Stokes equation: f = (-6*x+3*x^2,6*y + 3*y^2)

clear, clc, format short, format compact
% Example: 
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp05h';
% Segment nrs. of outer boundary = [1,2,3,4]
% -- Parameters ---------------
NU      = 1; % coeff. of viscosity (normed)
EPSILON = 0;         % Penalty term
Parmeter = [NU,EPSILON];
OPTION_MESH = 1; REFINE = 3; % Number of uniform mesh refinements
%OPTION_MESH = 2; REFINE = 4; % Number of uniform mesh refinements
% -----------------------------
Start = 100; KK = [0,1];
while ~ismember(Start,KK)
   Start = input(' New start or Restart ? (1/0) ');
end
if Start == 1, OPTION = 2;
   [p,e,t,p1,t1] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   save daten5 p e t p1 t1 
end
load daten5 p e t p1 t1
% Boundary values may be changed with restart only 
[RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1);
N = size(p,2) + size(p1,2);
RD = [RDU(1,:), RDV(1,:) + N,RDP(1)+2*N;
      RDU(2,:), RDV(2,:)    ,RDP(2)];
% ------------------------------
[U,V,P] = stokes1a(p,p1,t,t1,FU,FV,RD,Parmeter);
save daten5 p e t p1 t1 FU FV U V P
disp(' Call bild05 ! ')
bild05
