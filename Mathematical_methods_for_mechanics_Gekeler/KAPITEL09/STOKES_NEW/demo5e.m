function demo5e
% E.W. Gekeler, Release 08.08.10 
% Example with exact solution after Boukir
% Taylor-Hood elements with convection term
% DAE problem after Gresho
% COMPLETE NEWTON METHOD, ALSO FOR P, LUMPED MASS MATRIX
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

clear, clc, format short, format compact
% Example:
FF1 = 'bsp05'; FF2 = 'bsp05g'; FF3 = 'bsp05h';
% Segment nrs. for outer boundary = [1,2,3,4]
% -- Parameters -------------------
b = 10;    % after Boukir
nu = 1/b;  % coeff. of viscosity [m*m/sec]

maxiter = 100;   % step number for time iteration, 100
DT      = 0.01  ;       % time step  
Parmeter = [nu,DT];
OPTION_MESH = 1; REFINE = 2; % number of uniform mesh refinements
%OPTION_MESH = 2; REFINE = 3; 
% ---------------------------------------------
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or Restart ? (1/0) ');
end
%Start = 1;
if Start == 1, 
   [p,e,t] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   [p1,e,t1]  = mesh06_t(p,e,t); % e is enlarged by a further row 8
   [S,C,D,MM,BB] = taylor_hood(p,t,p1,t1);
   N1 = size(p,2); N = N1 + size(p1,2); NULL = zeros(N,N);
   % -- Dirichlet boundary conditions --------------------
   [RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1,b);
   RD = [RDU(1,:),RDV(1,:) + N, 2*N + RDP(1);
         RDU(2,:),RDV(2,:), RDP(2)];
   FF = [MM*FU;MM*FV]; 
      % -- Start for U and V ----
   AUX = zeros(1,2*N); AUX(RD(1,1:end-1)) = 1;
   AUXCOMP = find(AUX == 0);
   C1 = [C.',D.'];
   RSIDE = - C1(:,RD(1,1:end-1))*RD(2,1:end-1).';
   C2 = C1(:,AUXCOMP);
   RR = C2\RSIDE;
   UUA = zeros(2*N,1); 
   UUA(AUXCOMP) = RR; UUA(RD(1,1:end-1)) = RD(2,1:end-1).';
   U0 = UUA(1:N); V0 = UUA(N+1:end);
   % -- Calculation of u_dot -------------
   A = [MM,    NULL,   - C; 
        NULL,    MM,   - D;
        C.' ,    D.'    zeros(N1,N1)];
   CC = triform1(p,p1,t,t1,U0,V0);
   DD = [nu*S, NULL; NULL, nu*S];  
   RSIDE = [FF - (DD + CC(1:2*N,1:2*N))*UUA; zeros(N1,1)];  clear CC DD     
   J = RD(1,:); RSIDE = RSIDE - A(:,J)*RD(2,:).'; 
   RSIDE(J) = RD(2,:).';
   A(J,:) = 0; A(:,J) = 0;
   AUX = zeros(size(A,1),1); AUX(RD(1,:)) = 1;
   A = spdiags(diag(A)+AUX,0,A);
   AUX = A\RSIDE; P0 = AUX(2*N+1:end); clear A C1 C2
   U_DOT = AUX(1:2*N); U_DOT(RD(1,1:end-1)) = 0;
   % -- lumped mass matrix -------------
   MM1 = diag(MM); MM = spdiags(MM1,0,N,N); 
   AA = [2*MM/DT + nu*S, NULL,         - C; 
         NULL,         2*MM/DT + nu*S, - D;
         C.' ,         D.',            zeros(N1,N1)];
   AA = sparse(AA);   
   UUA = [UUA;P0];   
   save daten5ea p e t p1 t1 FF3 
   save daten5ec AA MM C D RD FF b UUA  
end  
load daten5ea p e t p1 t1 FF3
load daten5ec AA MM C D RD FF b UUA  
N1 = size(p,2); N = N1 + size(p1,2); N2 = 2*N+N1;
% -- starting values --------------
if Start == 1 
   UU0 = UUA; UU0(RD(1,:)) = RD(2,:).'; 
else
   load daten5fb UU0 U_DOT 
end
UU1 = UU0;
tic
for I = 1:maxiter  
   RSIDE1 = [MM*(2*UU0(1:N)/DT     + U_DOT(1:N))      + FF(1:N);
             MM*(2*UU0(N+1:2*N)/DT + U_DOT(N+1:2*N))  + FF(N+1:2*N);
             zeros(N1,1)];
   for JJ = 1:4  % 5 bringt nichts
      UU2 = UU1; UU2(RD(1,:)) = 0;
      [CC,DD] = triform5(p,p1,t,t1,UU1(1:N),UU1(N+1:2*N),UU2(1:N),UU2(N+1:2*N));
      BB = AA + [[CC;zeros(N1,2*N)],zeros(N2,N1)];
      CC = BB + [[DD;zeros(N1,2*N)],zeros(N2,N1)]; CC = sparse(CC);
      J = RD(1,:); 
      RSIDE2 = RSIDE1 - BB(:,J)*RD(2,:).';
      RSIDE2(J) = RD(2,:).';
      BB(J,:) = 0; BB(:,J) = 0;
      AUX = zeros(N2,1); AUX(RD(1,:)) = 1;
      BB = spdiags(diag(BB)+AUX,0,BB);
      RSIDE = BB*UU1 - RSIDE2;
      CC(J,:) = 0; CC(:,J) = 0;
      AUX = zeros(N2,1); AUX(RD(1,:)) = 1;
      CC = spdiags(diag(CC)+AUX,0,CC);
      DUU = CC\RSIDE; DUU(RD(1,:)) = 0;
      UU1 = UU1 - DUU; 
   end
   U_DOT = 2*(UU1(1:2*N) - UU0(1:2*N))/DT - U_DOT;
   DIFF_N = max(abs(DUU))
   %RESMAX = max(abs(RSIDE))
   DIFF = max(abs(UU1 - UU0));
   I_DIFF = [I,DIFF]
   UU0 = UU1;  
end
toc
save daten5eb UU0 U_DOT 
disp(' Call bild05 and select demo5e ! ')
clear all