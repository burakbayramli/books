function demo2
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: LID DRIVEN CAVITY
% Linear, Mini elements
% X,Y : coordinates
% U  :  velocity in x-direction
% V   : velocity in y-direction
% P   : Pressure (Bad result)
% p,e,t:  nodes,edges,triangles
% FF1   : File for first mesh (dropped in using MATLAB TOOLBOX)
% FF2   : File for geometry in MATLAB format 
% FF3   : File for boundary conditions   
% OPTION = 1/2 Mesh without/with MATLAB TOOLBOX
% Specify one node value for pressure 
% Right side F = 0;

clear, clc, format short, format compact
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp02h';
% -- Parameters ---------------
NU       = 1; % coeff. of viscosity (normed)
F        = 0;    % load
SEGNR    = [1,2,3,4];
EPSILON  = 0.1; % Penalty Term
Parmeter = [NU,F,EPSILON];
OPTION_MESH = 1; REFINE = 3;
% -----------------------------
Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' New start or Restart ? (1/0) ');
end
if Start == 1
   [p,e,t] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   save daten2 p e t 
end
load daten2 p e t 
[RDU,RDV,RDP] = feval(FF3,p,e);
[U,V,P,ecode] = stokes2(p,t,RDU,RDV,RDP,Parmeter);
save daten2 p e t  U V P
disp(' Call bild02 ! ');
