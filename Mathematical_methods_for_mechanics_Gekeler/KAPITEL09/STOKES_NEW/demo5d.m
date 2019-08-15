function demo5d
% E.W. Gekeler, Release 09.09.09 
% Example with exact solution after Boukir
% Taylor-Hood elements with convection term
% DAE problem after Gresho
% EXPLICIT EULER METHOD
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

maxiter = 500;   % step number for time iteration,
                 % at least 1000 steps
DT      = 0.001;       % time step  
Parmeter = [nu,DT];
OPTION_MESH = 1; REFINE = 2; % number of uniform mesh refinements
%OPTION_MESH = 2; REFINE = 4; 
% ---------------------------------------------
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or Restart ? (1/0) ');
end
%Start = 1;
if Start == 1, 
   [p,e,t] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   [p1,e,t1]  = mesh06_t(p,e,t); % e is enlarged by a further row 8

   [S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
   N1 = size(p,2); N = N1 + size(p1,2); NULL = zeros(N,N);
   AA = [MM/DT + nu*S, NULL,         -C; 
         NULL,         MM/DT + nu*S, -D;
         C.' ,         D.',          zeros(N1,N1)];
   AA = sparse(AA);      
   % -- Dirichlet boundary conditions --------------------
   [RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1,b);
   RD = [RDU(1,:),RDV(1,:) + N, 2*N + RDP(1);
         RDU(2,:),RDV(2,:), RDP(2)];
   FF = [MM*FU;MM*FV]; %FF(RD(1,:)) = 0;
   % -- Start for U and V ----
   AUX = zeros(1,N); AUX(RDU(1,:)) = 1;
   AUXU = find(AUX == 0); 
   C1 = C.'; C2 = C1(:,RDU(1,:)); C3 = C1(:,AUXU);
   AUX = zeros(1,N); AUX(RDV(1,:)) = 1;
   AUXV = find(AUX == 0); 
   D1 = D.'; D2 = D1(:,RDV(1,:)); D3 = D1(:,AUXV);
   RSIDE = C2*(RDU(2,:).')  + D2*(RDV(2,:).');
   BB = [C3,D3];
   UV = BB\RSIDE; clear BB
   U0 = zeros(N,1);
   U0(AUXU) = UV(length(AUXU)); U0(RDU(1,:)) = RDU(2,:).';
   V0 = zeros(N,1);
   V0(AUXV) = UV(length(AUXU)+1:end); UV(RDV(1,:)) = RDV(2,:).';
   UUA = [U0;V0];
   save daten5da p e t p1 t1 FF3 
   save daten5dc AA MM RD FF b UUA
end  
load daten5da p e t p1 t1 FF3
load daten5dc AA MM RD FF b UUA
N = size(p,2) + size(p1,2); NULL = zeros(N,N);
% -- starting values --------------
if Start == 1 
   UU0 = UUA;
else
   load daten5db U V 
   UU0 = [U;V];
end
for I = 1:maxiter  
   UU0(RD(1,1:end-1)) = RD(2,1:end-1); 
   A = AA + triform1a(p,p1,t,t1,UU0(1:N),UU0(N+1:2*N));
   
   RSIDE = [MM*UU0(1:N)/DT + FF(1:N);
            MM*UU0(N+1:2*N)/DT + FF(N+1:2*N);
            zeros(size(p,2),1)];
   J = RD(1,:); RSIDE = RSIDE - A(:,J)*RD(2,:).'; 
   RSIDE(J) = RD(2,:).';
   A(J,:) = 0; A(:,J) = 0;
   AUX = zeros(size(A,1),1); AUX(RD(1,:)) = 1;
   A = spdiags(diag(A)+AUX,0,A);
   AUX = A\RSIDE;
   UU1 = AUX(1:2*N); 
   DIFF = max(abs(UU1 - UU0));
   I_DIFF = [I,DIFF]
   UU0 = UU1; 
end
U = UU0(1:N); V = UU0(N+1:2*N); P = AUX(2*N+1:2*N+size(p,2)); 

save daten5db U V P 
disp(' Call bild05! ')
