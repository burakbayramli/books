function demo5c
% as demo5b.m but implicit condition for pressure
% E.W. Gekeler, Release 09.09.09 

% implicit pressure condition, bad results

% Example with exact solution after Boukir
% Taylor-Hood elements with convection term
% DAE problem with semi-explicit Runge-Kutta method
% X,Y : coordinates
% U,V :  velocity in x- and y-direction
% P   :  Pressure
% p,e,t: nodes,edges,triangles
% p1,t1: data of midpoints and their indices in triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]
% FF1 : File for first mesh (not used in MATLAB TOOLBOX)
% FF2 : File for geometry in MATLAB format 
% FF3 : File for boundary conditions and loads  
% OPTION_MESH = 1/2 Mesh without/with MATLAB TOOLBOX
% specify one node value for pressure 
% acceptable pressure for OPTION_MESH = 2, REFINE = 4;
% HERE WITH LUMPED MASS MATRIX

clear, clc, format short, format compact
% Example:
FF1 = 'bsp05'; FF2 = 'bsp05g'; FF3 = 'bsp05h';
% Segment nrs. for outer boundary = [1,2,3,4]
% -- Parameters -------------------
b = 10;  % after Boukir
nu = 1/b;      % coeff. of viscosity [m*m/sec]

maxiter = 500;   % step number for time iteration, 2000
DT      = 0.001;       % time step 

DT = 0.0005; %!!!!!!!!!!!!!!!!
Parmeter = [nu,DT];
OPTION_MESH = 1; REFINE = 2; % number of uniform mesh refinements
%OPTION_MESH = 2; REFINE = 4; 
%-- Data of semi-explicit Runge-Kutta method --
alfa21 = 1/3; alfa31 = -1; alfa32 = 2;
beta1 = 0; beta2 = 3/4; beta3 = 1/4;
% ---------------------------------------------
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or Restart ? (1/0) ');
end
%Start = 1;
if Start == 1, 
   [p,e,t] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   [p1,e,t1]  = mesh06_t(p,e,t); % e is enlarged by a further row 8

   [S,C,D,MM,B_LIN,S_LIN] = taylor_hood(p,t,p1,t1);
   N = size(p,2) + size(p1,2); NULL = zeros(N,N);
   % -- Dirichlet boundary conditions --------------------
   [RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1,b);
   RD = [RDU(1,:),RDV(1,:) + N;
         RDU(2,:),RDV(2,:)];
   FF = [MM*FU;MM*FV]; FF(RD(1,:)) = 0;
   MM1 = diag(MM); MM2 = MM1;
   MM1(RDU(1,:)) = 1; MM2(RDV(1,:)) = 1; 
   MM3 = ones(2*N,1)./[MM1;MM2]; 
   INVMM    = spdiags(MM3,0,2*N,2*N); 
   MATRIXP  = [C;D];     
   AUX      = MATRIXP.'*INVMM;
   MATRIXP1 = MATRIXP; MATRIXP1(RD(1,:)) = 0;
   GRADP    = MATRIXP.'*INVMM*MATRIXP1; 
   BB = zeros(size(p,2),1); BB(RDP(1)) = 1;
   GRADP = [GRADP,BB];
   GRADP = [GRADP;[BB.',0]];
   % -- boundary data and loads ------------
   save daten5ca p e t p1 t1 FF3 S INVMM AUX GRADP MATRIXP RD FF b RDP
end  
disp(' NU may be changed without new start ')
load daten5ca p e t p1 t1 FF3 S INVMM AUX GRADP MATRIXP RD FF b RDP
N = size(p,2) + size(p1,2); NULL = zeros(N,N);
% -- starting values --------------
if Start == 1 % cold start
   U = zeros(N,1); V = U; P = zeros(size(p,2),1);
else
   load daten5cb U V P
end
UU0 = [U;V]; P0 = P;
for I = 1:maxiter  
   UU0(RD(1,:)) = RD(2,:); 
   A1 = triform3(p,p1,t,t1,UU0,S,nu);
   %A1(RD(1,:),:) = 0;
   RSIDEP1 = (- MATRIXP.'*UU0 - DT*alfa21*AUX*(- A1*UU0 + FF))/(DT*alfa21);
   RSIDEP1 = [RSIDEP1;RDP(2)];
   P1 = GRADP\RSIDEP1; 
   P1 = P1(1:end-1);
   FF1 = -A1*UU0 + MATRIXP*P1 + FF; FF1(RD(1,:)) = 0;
   UU1 = UU0 + DT*alfa21*INVMM*FF1;
   A2 = triform3(p,p1,t,t1,UU1,S,nu);
   %A2(RD(1,:),:) = 0;
   RSIDEP2 = - MATRIXP.'*UU0 - DT*AUX*(alfa31*FF1 + alfa32*(-A2*UU1 + FF)); 
   RSIDEP2 = [RSIDEP2;RDP(2)];
   P2 = GRADP\RSIDEP2;
   P2 = P2(1:end-1);
   FF2 = -A2*UU1 + MATRIXP*P2 + FF; FF2(RD(1,:)) = 0;
   UU2 = UU0 + DT*INVMM*(alfa31*FF1 + alfa32*FF2);  
   A3 = triform3(p,p1,t,t1,UU2,S,nu);
   %A3(RD(1,:),:) = 0;
   RSIDEP3 = - MATRIXP.'*UU0 - DT*AUX*(beta2*FF2 + beta3*(- A3*UU2 + FF));    
   RSIDEP3 = [RSIDEP3;RDP(2)];       
   P3 = GRADP\RSIDEP3;
   P3 = P3(1:end-1);
   FF3 = -A3*UU2 + MATRIXP*P3 + FF; FF3(RD(1,:)) = 0;
   UU3 = UU0 + DT*INVMM*(beta2*FF2 + beta3*FF3);    
%   DIFF = max(abs(P3 - P0));
   DIFF = max(abs(UU3 - UU0));

   %I
   %DIFF
   I_DIFF = [I,DIFF]
   UU0 = UU3; P0 = P3;
end
U = UU0(1:N); V = UU0(N+1:2*N); P = P3;

save daten5cb U V P 
disp(' Call bild05 and select demo5c ! ')
