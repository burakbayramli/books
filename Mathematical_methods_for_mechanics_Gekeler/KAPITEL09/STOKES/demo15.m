function demo15
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: BACKFACING STEP
% Taylor-Hood elements with convection term
% X,Y : coordinates
% U,V : velocity in x- and y-direction
% P   : Pressure
% p,e,t: nodes,edges, triangles
% p1,t1: data of intermediate nodes and their indices in triangles
% e is augmented by an eighth row with numbers of midpoints in [p,p1]
% FF1   : File for first mesh (dropped in using MATLAB TOOLBOX)
% FF2   : File for geometry in MATLAB format 
% FF3   : File for boundary conditions   
% OPTION_MESH = 1/2 Mesh without/with MATLAB TOOLBOX
% OPTION_P = 1: Specify one node value for pressure 
% Sequence for NU: New start with 0.1
% Restart with 0.01,0.005

clear, clc, format short, format compact
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global LEADING_MATRIX_A  RIGHTSIDE_B  BOUNDARY_DATA
global p e t p1 t1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example: 
FF1 = 'bsp09'; FF2 = 'bsp09g'; FF3 = 'bsp09h';
% Segment nrs. for outer boundary:
SEGNR  = [6 7 8 9 14 19 24 29 34 39 44 45 42 43 38 33 28 23 18 13 3 5];

% -- Parameters ---------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nu = 0.1;     % coeff. of viscosity
disp(' Sequence for NU: New start:0.1,Restart:0.01,0.005')

OPTION_MESH = 1; REFINE = 2; % Number of uniform mesh refinements
%OPTION_MESH = 2; REFINE = 2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
epsilon = 0.1;         % weight for penalty term
Parmeter = [nu,epsilon];
maxiter  = 10;   % max. step number for Newton method
tol      = 1E-5; % Tolerance for Newton method

Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' New start or Restart? (1/0) ');
end
%Start = 1;
if Start == 1, 
   [p,e,t,p1,t1]   = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   [S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
   N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
   % -- boundary data and loads
   [RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1);
   B = [MM*FU;MM*FV;zeros(N1,1)]; % right side 
   RIGHTSIDE_B = B; % for Newton method 
   save daten15a p e t p1 t1 S C D S_LIN B
end  
disp(' Boundary conditions and NU may be changed with restart only ')
load daten15a p e t p1 t1 S C D S_LIN B
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
A = [nu*S      , zeros(N,N), -C;
     zeros(N,N), nu*S      , -D;
     C.'        , D.'        , epsilon*S_LIN];
LEADING_MATRIX_A = sparse(A); % global variable for Newton method 
RIGHTSIDE_B = B;              % global variable for Newton method 
% -- boundary values  ----
[RDU,RDV,RDP] = feval(FF3,p,e,p1);
RD = [RDU(1,:),RDV(1,:) + N,RDP(1,:) + 2*N;
      RDU(2,:),RDV(2,:)    ,RDP(2,:)];
BOUNDARY_DATA = RD;           % global variable for Newton method
% -- starting values --------------
if Start == 1
   U = ones(N,1); V = U; P = zeros(N1,1);
else
   load daten15b U V P
end
% -- boundary conditions ---------------------
U(RDU(1,:)) = RDU(2,:).'; V(RDV(1,:)) = RDV(2,:).'; 
P(RDP(1)) = RDP(2);
X0 = [U;V;P]; Parmeter = [];
[X,errorcode] = newton('newtonaux1',X0,maxiter,tol,Parmeter);
U = X(1:N); V = X(N+1:2*N); P = X(2*N+1:2*N+N1);
save daten15b U V P 
disp(' bild09 is called! ')
bild09
