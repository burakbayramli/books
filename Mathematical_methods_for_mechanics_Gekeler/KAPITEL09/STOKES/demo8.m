function demo8
% same as demo7.m but without global variables to save memory  
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: LID DRIVEN CAVITY
% Taylor-Hood elements with convection term
% quasi-global NEWTON method
% X,Y : coordinates
% U,V :  velocity in x- and y-direction
% P   :  Pressure
% p,e,t: nodes,edges,triangles
% p1,t1: data of intermediate nodes and their indices in triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]
% FF1 : File for first mesh (not used in MATLAB TOOLBOX)
% FF2 : File for geometry in MATLAB format 
% FF3 : File for boundary conditions and loads  
% OPTION_MESH = 1/2 Mesh without/with MATLAB TOOLBOX
% Specify one node value for pressure 
% Acceptable pressure for OPTION = 2, REFINE = 5;
% Sequence for NU: New start with 0.01
% Restart with 0.005,0.001,0.0005

clear, clc, format short, format compact
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%global LEADING_MATRIX_A  RIGHTSIDE_B  
%global p e t p1 t1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example:
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h_1'; % not bsp01h_2.m
% Segment nrs. for outer boundary = [1,2,3,4]  
% -- Parameters -------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp(' Sequence for NU: New start:0.01,Restart:0.005,0.001,0.0005,0.0003 ')
nu      = 0.01;      % coeff. of viscosity [m*m/sec]
%OPTION_MESH = 2; REFINE = 5; % number of uniform mesh refinements
OPTION_MESH = 1; REFINE = 3; % number of uniform mesh refinements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
epsilon = 0.1;  % Parameter for stabilization
maxiter  = 10;   % max. step number for Newton method
tol      = 1E-5; % Tolerance for Newton method
% -------------------------------
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or Restart ? (1/0) ');
end
%Start = 1;
if Start == 1, 
   [p,e,t,p1,t1]   = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   [S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
   N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
   % Call boundary data and loads
   [RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1);
   B = [MM*FU;MM*FV;zeros(N1,1)]; % right side 
  % RIGHTSIDE_B = B; % for Newton method 
   save daten8a p e t p1 t1 S C D S_LIN B
   clear p e t p1 t1 S C D S_LIN B
end  
tic
disp(' Boundary conditions and NU may be changed with restart only ')
load daten8a p e t p1 t1 S C D S_LIN B
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
A = [nu*S      , zeros(N,N), -C;
     zeros(N,N), nu*S      , -D;
     C.'        , D.'        , epsilon*S_LIN];
% -- Dirichlet boundary conditions --------------------
[RDU,RDV,RDP] = feval(FF3,p,e,p1);
RD = [RDU(1,:),RDV(1,:) + N,RDP(1,:) + 2*N;
      RDU(2,:),RDV(2,:)    ,RDP(2,:)];
N3 = size(RD,2);   

% -- matrix and right side augmented by boundary conditions     
boundary = zeros(N3,2*N + N1);
for i = 1:N3, boundary(i,RD(1,i)) = 1; end        
A = [A, boundary.';
    boundary, zeros(N3,N3)];
B = [B;RD(2,:).'];          
save daten8c A B 
clear p e t p1 t1 S C D S_LIN B
clear A B
% -- starting values --------------
if Start == 1
   U = ones(N,1); V = U; P = zeros(N1,1); Y = zeros(N3,1);
else
   load daten8b U V P Y
end
X0 = [U;V;P;Y]; Parmeter = [];
[X,errorcode] = newton('newtonaux4',X0,maxiter,tol,Parmeter);
U = X(1:N); V = X(N+1:2*N); P = X(2*N+1:2*N+N1); Y = X(2*N+N1+1:2*N+N1+N3);
toc
save daten8b U V P Y
disp(' bild06 is called! ')
bild06
