function demo13
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: BACKFACING STEP
% Taylor-Hood elements with convection term,
% Newton method
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
% Sequence for NU: New start with 0.1, Restart with 0.01
% U0 velocity constant at inflow

clear, clc, format short, format compact
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global LEADING_MATRIX_A  RIGHTSIDE_B  BOUNDARY_DATA
global p e t p1 t1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example: 
FF1 = 'bsp09'; FF2 = 'bsp09g'; FF3 = 'bsp09h'; U0 = 10;
% Segment nrs. for outer boundary:
SEGNR  = [6 7 8 9 14 19 24 29 34 39 44 45 42 43 38 33 28 23 18 13 3 5];
% -- Parameters -------------------
scaled = 1;  % unscaled or scaled problem 0/1

switch scaled
case 0, %unscaled problem
   nu      = 0.1; % first nu = 0.1 then nu = 0.01, coeff. of viscosity [m*m/sec]
   %nu = 0.01;
   U0      = 100;      % velocity at inflow
   scale_factor = 1;   % for geometry
case 1,% scaled problem
   nu = 0.1;  % start with nu = 0.1, restart with nu = 0.05
   % nu = 0.05;
   U0 = 10;
   scale_factor = 50;  %= 1/0.02;    
end
disp(' Sequence for NU: New start:0.1,Restart:0.01,0.005')

OPTION_MESH = 1; REFINE = 2; % Number of uniform mesh refinements
%OPTION_MESH = 2; REFINE = 2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
epsilon = 0.1;         % weight for penalty term
parmeter = [nu,epsilon,U0,scale_factor,scaled];

maxiter  = 10;   % max. step number for Newton method
tol      = 1E-5; % Tolerance for Newton method

Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' New start or Restart? (1/0) ');
end
%Start = 1;
if Start == 1, 
   [p,e,t]   = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   p = p*scale_factor; 
   %bild00(p,e,t), pause
   [p1,e,t1] = mesh06_t(p,e,t); % e is enlarged by a further row 8

   [S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
   N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
   % -- boundary data and loads
   [RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1,parmeter);
   B = [MM*FU;MM*FV;zeros(N1,1)]; % right side 
   RIGHTSIDE_B = B; % for Newton method 
   save daten13a p e t p1 t1 FF3 parmeter
   save daten13c S C D S_LIN B 
end  
disp(' Boundary conditions and NU may be changed with restart only ')
load daten13a p e t p1 t1 FF3 parmeter 
load daten13c S C D S_LIN B 

N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
A = [nu*S      , zeros(N,N), -C;
     zeros(N,N), nu*S      , -D;
     C.'        , D.'        , epsilon*S_LIN];
LEADING_MATRIX_A = sparse(A); % global variable for Newton method 
RIGHTSIDE_B = B;              % global variable for Newton method 
% -- boundary values  ----
[RDU,RDV,RDP] = feval(FF3,p,e,p1,parmeter);
RD = [RDU(1,:),RDV(1,:) + N,RDP(1,:) + 2*N;
      RDU(2,:),RDV(2,:)    ,RDP(2,:)];
BOUNDARY_DATA = RD;           % global variable for Newton method
% -- starting values --------------
if Start == 1
   U = ones(N,1); V = U; P = zeros(N1,1);
else
   load daten13b U V P
end
% -- boundary conditions ---------------------
U(RDU(1,:)) = RDU(2,:).'; V(RDV(1,:)) = RDV(2,:).'; 
P(RDP(1)) = RDP(2);
X0 = [U;V;P]; Parmeter = [];
[X,errorcode] = newton('newtonaux1',X0,maxiter,tol,parmeter);
U = X(1:N); V = X(N+1:2*N); P = X(2*N+1:2*N+N1);
save daten13b U V P 
disp(' Call bild09 and select demo13 ! ')
