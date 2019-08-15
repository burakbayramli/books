function demo10
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: FLOW PAST HALF CYLINDER
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
% U0 velocity constant at inflow

clear, clc, format short, format compact
% Example:
FF1 = 'bsp03_3'; FF2 = 'bsp03g_3'; FF3 = 'bsp03h_3'; U0 = 0.1;
% Segment nrs. for boundary = [1,2,3,4,5,6,7,8]
% -- Parameters -------------------
nu      = 0.0001;  % coeff. of viscosity [m*m/sec]
maxiter = 500;   % step number for time iteration, 600 steps
DT      = 0.05;       % time step  
Parmeter = [nu,DT];
OPTION_MESH = 1; REFINE   = 2; % number of uniform mesh refinements
OPTION_MESH = 2; REFINE   = 3; % number of uniform mesh refinements
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
   [p,e,t]   = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   % partial refinement:
   if OPTION_MESH == 2
      XL = 0; XR = 2; YU = 0; YO = 1;
      J = find(p(1,:) > XL & p(1,:) < XR);
      K = find(p(2,:) > YU & p(2,:) < YO);
      K = intersect(J,K);
      L1= ismember(t(1,:),K); L1 = find(L1 == 1);
      L2= ismember(t(2,:),K); L2 = find(L2 == 1);
      L3= ismember(t(3,:),K); L3 = find(L3 == 1);
      L = unique([L1,L2,L3]);
       [p,e,t] = refinemesh(FF2,p,e,t,L.','regular');
   end
   %bild00(p,e,t), pause
   [p1,e,t1] = mesh06_t(p,e,t); % e is enlarged by a further row 8

   [S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
   N = size(p,2) + size(p1,2); NULL = zeros(N,N);
   % -- Dirichlet boundary conditions --------------------
   [RDU,RDV,RDP] = feval(FF3,p,e,p1,U0);
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
   save daten10a p e t p1 t1 FF3 U0 
   save daten10c S INVMM AUX GRADP MATRIXP RD RDP 
end  
disp(' NU may be changed without new start ')
load daten10a p e t p1 t1
load daten10c S INVMM AUX GRADP MATRIXP RD RDP 
N = size(p,2) + size(p1,2); NULL = zeros(N,N);
% -- starting values --------------
if Start == 1 % cold start
   U = zeros(N,1); V = U; 
else
   load daten10b U V 
end
UU0 = [U;V]; 
for i = 1:maxiter  
   UU0(RD(1,:)) = RD(2,:); 
   A1 = triform3(p,p1,t,t1,UU0,S,nu);
   A1(RD(1,:),:) = 0;
   RSIDEP1 = (- MATRIXP.'*UU0 + DT*alfa21*AUX*A1*UU0)/(DT*alfa21);
   RSIDEP1(RDP(1)) = RDP(2);
   P1 = GRADP\RSIDEP1; 
   FF1 = -A1*UU0 + MATRIXP*P1; FF1(RD(1,:)) = 0;
   UU1 = UU0 + DT*alfa21*INVMM*FF1;
   A2 = triform3(p,p1,t,t1,UU1,S,nu);
   A2(RD(1,:),:) = 0;
   RSIDEP2 = - MATRIXP.'*UU0 - DT*AUX*(alfa31*FF1 - alfa32*A2*UU1); 
   RSIDEP2 = RSIDEP2/(DT*alfa32);
   RSIDEP2(RDP(1)) = RDP(2);
   P2 = GRADP\RSIDEP2;
   FF2 = -A2*UU1 + MATRIXP*P2; FF2(RD(1,:)) = 0;
   UU2 = UU0 + DT*INVMM*(alfa31*FF1 + alfa32*FF2);  
   A3 = triform3(p,p1,t,t1,UU2,S,nu);
   A3(RD(1,:),:) = 0;
   RSIDEP3 = - MATRIXP.'*UU0 - DT*AUX*(beta2*FF2 - beta3*A3*UU2);    
   RSIDEP3 = RSIDEP3/(DT*beta3);       
   RSIDEP3(RDP(1)) = RDP(2);
   P3 = GRADP\RSIDEP3;
   FF3 = -A3*UU2 + MATRIXP*P3; FF3(RD(1,:)) = 0;
   UU3 = UU0 + DT*INVMM*(beta2*FF2 + beta3*FF3);    
   DIFF = max(abs(UU3-UU0));
   ITER_DIFFU = [i,DIFF]
   UU0 = UU3; 
end
U = UU0(1:N); V = UU0(N+1:2*N); P = P3;
save daten10b U V P 
disp(' Call bild07 and select demo10 ! ')
