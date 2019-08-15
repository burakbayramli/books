function demo14
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: BACKFACING STEP
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
% acceptable pressure for OPTION_MESH = 2, REFINE = 2;
% HERE WITH LUMPED MASS MATRIX
% U0 velocity at inflow

clear, clc, format short, format compact

% Example: 
FF1 = 'bsp09'; FF2 = 'bsp09g'; FF3 = 'bsp09h';
% Segment nrs. for outer boundary:
SEGNR  = [6 7 8 9 14 19 24 29 34 39 44 45 42 43 38 33 28 23 18 13 3 5];
% -- Parameters -------------------
scaled = 1;  % unscaled or scaled problem 0/1

switch scaled
case 0 % unscaled problem
   nu  = 1.338E-5; % coeff. of viscosity [m*m/sec]
   U0  = 10;      % velocity at inflow
   DT  = 1E-6;     % time step  
   scale_factor = 1;
   USTART = 0;
case 1, %scaled problem
   nu = 1/14950; % = 6.7E-5
   U0 = 10  ;
   DT = 1E-3;
   scale_factor = 50;
   USTART = 0;
case 2 % scaled and same as in demo13.m (stationary problem)
   nu = 0.1;
   U0 = 10;
   DT = 0.01;
   scale_factor = 50;
   USTART = 0;
end   

maxiter = 200;      % step number for time iteration total 500

parmeter = [nu,DT,U0,scale_factor,scaled];

OPTION_MESH = 1; REFINE = 2; % Number of uniform mesh refinements
%OPTION_MESH = 2; REFINE = 2;
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
   
   % partial refinement:
   part_ref = 0;
   if part_ref == 1
      XL = 0.04; XR = 0.1; 
      J = find(p(1,:) > XL) ;
      K = find(p(1,:) < XR);
      K = intersect(J,K);
      L1= ismember(t(1,:),K); L1 = find(L1 == 1);
      L2= ismember(t(2,:),K); L2 = find(L2 == 1);
      L3= ismember(t(3,:),K); L3 = find(L3 == 1);
      L = unique([L1,L2,L3]);
       [p,e,t] = refinemesh(FF2,p,e,t,L.','regular');
   end
   p = p*scale_factor; 
   %bild00(p,e,t), pause

   [p1,e,t1] = mesh06_t(p,e,t); % e is enlarged by a further row 8
   % bild00(p,e,t), pause
   [S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
   N = size(p,2) + size(p1,2); NULL = zeros(N,N);
   % -- Dirichlet boundary conditions --------------------
   [RDU,RDV,RDP] = feval(FF3,p,e,p1,parmeter);
   RD = [RDU(1,:),RDV(1,:) + N;
         RDU(2,:),RDV(2,:)];
   MM1 = diag(MM); MM2 = MM1;
   MM1(RDU(1,:)) = 1; MM2(RDV(1,:)) = 1; 
   MM3 = ones(2*N,1)./[MM1;MM2]; 
   INVMM    = spdiags(MM3,0,2*N,2*N); 
   MATRIXP  = [C;D];     
   AUX      = MATRIXP.'*INVMM;
   MATRIXP1 = MATRIXP; MATRIXP1(RD(1,:)) = 0;
   GRADP    = MATRIXP.'*INVMM*MATRIXP1; 
   J  = RDP(1);
   GRADP(J,:) = 0; GRADP(:,J) = 0; GRADP(J,J) = 1; 
   % -- boundary data and loads ------------
   save daten14a p e t p1 t1 FF3 parmeter
   save daten14c S INVMM AUX GRADP MATRIXP RD RDP 
end  
disp(' NU may be changed without new start ')
load daten14a p e t p1 t1 FF3 parmeter
load daten14c S INVMM AUX GRADP MATRIXP RD RDP
N = size(p,2) + size(p1,2); NULL = zeros(N,N);
% -- starting values --------------
if Start == 1 % cold start
   U = USTART*ones(N,1); V = zeros(N,1); 
else
   load daten14b U V 
end
UU0 = [U;V]; 
for i = 1:maxiter  
   UU0(RD(1,:)) = RD(2,:); 
   A = triform3(p,p1,t,t1,UU0,S,nu);
  % A(RD(1,:),:) = 0;
   RSIDEP = (- MATRIXP.'*UU0 + DT*alfa21*AUX*A*UU0)/(DT*alfa21);
   RSIDEP(RDP(1)) = RDP(2);
   P1 = GRADP\RSIDEP; 
   FF1 = -A*UU0 + MATRIXP*P1; FF1(RD(1,:)) = 0;
   UU1 = UU0 + DT*alfa21*INVMM*FF1;
   A = triform3(p,p1,t,t1,UU1,S,nu);
  % A(RD(1,:),:) = 0;
   RSIDEP = - MATRIXP.'*UU0 - DT*AUX*(alfa31*FF1 - alfa32*A*UU1); 
   RSIDEP = RSIDEP/(DT*alfa32);
   RSIDEP(RDP(1)) = RDP(2);
   P2 = GRADP\RSIDEP;
   FF2 = -A*UU1 + MATRIXP*P2; FF2(RD(1,:)) = 0;
   UU2 = UU0 + DT*INVMM*(alfa31*FF1 + alfa32*FF2);  
   A = triform3(p,p1,t,t1,UU2,S,nu);
   %A(RD(1,:),:) = 0;
   RSIDEP = - MATRIXP.'*UU0 - DT*AUX*(beta2*FF2 - beta3*A*UU2);    
   RSIDEP = RSIDEP/(DT*beta3);       
   RSIDEP(RDP(1)) = RDP(2);
   P3 = GRADP\RSIDEP;
   FF3 = -A*UU2 + MATRIXP*P3; FF3(RD(1,:)) = 0;
   UU3 = UU0 + DT*INVMM*(beta2*FF2 + beta3*FF3);    
   DIFF = max(abs(UU3-UU0));
   ITER_DIFFU = [i,DIFF]
   UU0 = UU3; 
end
U = UU0(1:N); V = UU0(N+1:2*N); P = P3;
save daten14b U V P 
disp(' Call bild09 and select demo14 ! ')
