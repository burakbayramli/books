function demo4
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: LID DRIVEN CAVITY
% Taylor-Hood elements with convection term
% simple nonlinear iteration
% X,Y : coordinates
% U,V :  velocity in x- and y-direction
% P   : Pressure
% p,e,t: nodes,edges,triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]
% p1,t1: data of intermediate nodes and their indices in triangles
% FF1  : File for first mesh (dropped in using MATLAB TOOLBOX)
% FF2  : File for geometry in MATLAB format 
% FF3  : File for boundary conditions   
% OPTION_MESH = 1/2 Mesh without/with MATLAB TOOLBOX

clear, clc, format short, format compact
% Example:
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h';
% Segment nrs. of outer boundary = [1,2,3,4]
% -- Parameters -------------------
%%%%%%%%%%%%%%%%%%%%%%%%%
MAXITER = 20;
%%%%%%%%%%%%%%%%%%%%%%%%%
disp(' new start: NU = 0.1, restart: 0.05, 0.025 ')
NU      = 0.1;  % coeff. of viscosity [m*m/sec]
EPSILON = 0.3;    % Penalty term
Parmeter = [NU,EPSILON];
%OPTION_MESH = 2; REFINE = 3; % Number of uniform mesh refinements
OPTION_MESH = 1; REFINE = 3; % Number of uniform mesh refinements
% -------------------------------
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or restart? (1/0) ');
end
if Start == 1  
   [p,e,t] = start4stokes(FF1,FF2,OPTION_MESH,REFINE);
   [p1,e,t1]  = mesh06_t(p,e,t); % e is enlarged by a further row 8
   %bild00(p,e,t)
   save daten4a p e t p1 t1 FF3
end
load daten4a p e t p1 t1 FF3
%disp( ' Boundary values and nu can be changed before restart ')
[RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1);
% -- initial values --------------
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
U = zeros(N,1); V = U; P = zeros(N1,1);
for ITER = 1:MAXITER
   CC = triform1(p,p1,t,t1,U,V);
   [UN,VN,PN] = stokes3(p,p1,t,t1,RDU,RDV,RDP,FU,FV,Parmeter,CC);
   DIFFU = max(abs(UN - U)); DIFFV  = max(abs(VN - V));
   DIFFP = max(abs(PN - P));
   U = UN; V = VN; P = PN;
   ITER_DIFFU_DIFFV_DIFF_P = [ITER,DIFFU, DIFFV,DIFFP]
end
save daten4b U V P     % Full Output
disp(' Call bild06 and select demo4 ! ')
