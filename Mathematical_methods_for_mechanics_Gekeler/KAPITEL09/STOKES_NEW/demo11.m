function demo11
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: FLOW PAST CYLINDER
% Taylor-Hood elements with convection term
% quasi-global NEWTON method
% X,Y : coordinates
% U/V :  velocity in x/y-direction
% P   :  Pressure
% p,e,t: knots,edges,triangles in vertices
% p1,t1: data of intermediate nodes and their indices in triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]
% FF1   : File for first mesh (omitted in MATLAB TOOLBOX)
% FF2   : File for geometry in MATLAB format 
% FF3   : File for boundary conditions   
% OPTION_MESH = 1/2: Mesh without/with MATLAB TOOLBOX
% Specify one node value for pressure 
% Acceptable pressure for OPTION = 2, REFINE = 5;
% Sequence for NU: New start with 0.1 
% Restart with 0.01,0.005,0.001,0.0005
% U0 velocity constant at inflow

clear, clc, format short, format compact
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global LEADING_MATRIX_A  RIGHTSIDE_B  BOUNDARY_DATA
global p e t p1 t1

% Example:
FF1 = 'bsp10b'; FF2 = 'bsp10gb'; FF3 = 'bsp10h'; U0 = 1;
% segment nrs. for outer boundary = [1,7,8,9,3,4]; 
% segment nrs. for inner boundary = [5,6]; 
% -- Parameters -------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nu      = 0.5;      % coeff. of viscosity [m*m/sec]
disp(' Sequence for NU: New start:0.5,Restart:0.1,0.05,0.03,0.025')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
epsilon  = 0.1; %1E-3;  % Stabilization
maxiter  = 10;   % max. step number for Newton method
tol      = 1E-5; % Tolerance for Newton method
OPTION_MESH = 1; REFINE   = 3; JIGGLE = 1; % number of uniform mesh refinements
%OPTION_MESH = 2; REFINE   = 3; % number of uniform mesh refinements
% -------------------------------
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or restart? (1/0) ');
end
%Start = 1;
if Start == 1 
   [p,e,t]   = start4stokes(FF1,FF2,OPTION_MESH,REFINE,JIGGLE); 
   [p1,e,t1] = mesh06_t(p,e,t); % e is enlarged by a further row 8

   %bild00(p,e,t), pause
   [S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
   N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
   % -- boundary data and right side F (here zero)
   [RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1,U0);
   B = [MM*FU;MM*FV;zeros(N1,1)]; % right side 
   RIGHTSIDE_B = B; % for Newton method 
   save daten11a p e t p1 t1
   save daten11c S C D S_LIN B
end
disp(' Boundary conditions and NU may be changed with restart only ')
load daten11a p e t p1 t1 
load daten11c B S C D S_LIN B
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
A = [nu*S      , zeros(N,N), -C;
     zeros(N,N), nu*S      , -D;
     C.'        , D.'        , epsilon*S_LIN];
LEADING_MATRIX_A = sparse(A); % global variable for Newton method 
RIGHTSIDE_B = B;              % global variable for Newton method 
% -- Dirichlet boundary conditions --------------------
[RDU,RDV,RDP] = feval(FF3,p,e,p1,U0);
RD = [RDU(1,:),RDV(1,:) + N,RDP(1,:) + 2*N;
      RDU(2,:),RDV(2,:)    ,RDP(2,:)];
BOUNDARY_DATA = RD;           % global variable for Newton method
% -- starting values --------------
if Start == 1
   U = ones(N,1); V = U; P = zeros(N1,1);
else
   load daten11b U V P
end
% -- boundary conditions ---------------------
U(RDU(1,:)) = RDU(2,:).'; V(RDV(1,:)) = RDV(2,:).'; 
P(RDP(1)) = RDP(2);
X0 = [U;V;P]; Parmeter = [];
[X,errorcode] = newton('newtonaux1',X0,maxiter,tol,Parmeter);
U = X(1:N); V = X(N+1:2*N); P = X(2*N+1:2*N+N1);
save daten11b U V P nu FF3 U0
disp(' Call bild10! ')
