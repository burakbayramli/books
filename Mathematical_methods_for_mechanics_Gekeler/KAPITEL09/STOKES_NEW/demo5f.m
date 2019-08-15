function demo5f
% E.W. Gekeler, Release 09.09.09 
% Example with exact solution after Boukir
% Taylor-Hood elements with convection term
% DAE problem after TUREK
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
FF1 = 'bsp05'; FF2 = 'bsp05g'; FF3 = 'bsp05h'; FF4 = 'bsp05k';
% Segment nrs. for outer boundary = [1,2,3,4]
% -- Parameters -------------------
b = 10;    % after Boukir
nu = 1/b;  % coeff. of viscosity [m*m/sec]

maxiter = 400;   % step number for time iteration,
                 % at least 800 steps
DT      = 0.001;       % time step orig 0.001  
Parmeter = [nu,DT];
OPTION_MESH = 1; REFINE = 2; % number of uniform mesh refinements
OPTION_MESH = 2; REFINE = 3; 
% ---------------------------------------------
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or Restart ? (1/0) ');
end
%Start = 1;
if Start == 1, 
   [p,e,t] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 
   [p1,e,t1]  = mesh06_t(p,e,t); % e is enlarged by a further row 8
   [S,C,D,MM,BB,AP] = taylor_hood(p,t,p1,t1);
   N1 = size(p,2); N = N1 + size(p1,2); NULL = zeros(N,N);
   % -- Dirichlet boundary conditions --------------------
   [RDU,RDV,RDP,FU,FV] = feval(FF3,p,e,p1,b);
   RD = [RDU(1,:),RDV(1,:) + N;
         RDU(2,:),RDV(2,:)];
   FF = [MM*FU;MM*FV]; 
   % -- lumped mass matrix -------------
   MM1 = diag(MM);
   MM = spdiags(MM1,0,N,N); 
   MM3 = ones(2*N,1)./[MM1;MM1]; 
   INVMM    = spdiags(MM3,0,2*N,2*N); 

   AA = [MM + DT*nu*S, NULL; 
         NULL,        MM + DT*nu*S];
   AA = sparse(AA);      
   J = RDP(1); AP(J,:) = 0; AP(:,J) = 0; % for pressure update
   AUX = zeros(size(AP,1),1); AUX(RDP(1)) = 1;
   AP = spdiags(diag(AP)+AUX,0,AP);
   CC = [C;D];
   save daten5fa p e t p1 t1 FF3 
   save daten5fc AA MM RD FF b CC DT AP RDP INVMM
end  
load daten5fa p e t p1 t1 FF3
load daten5fc AA MM RD FF b CC DT AP RDP INVMM
N1 = size(p,2); N = N1 + size(p1,2); 
% -- starting values --------------
if Start == 1 
   UU0 = zeros(2*N,1); P = zeros(N1,1);
else
   load daten5fb U V P
   UU0 = [U;V];
end
for I = 1:maxiter 
   UU0(RD(1,:)) = RD(2,:); 
   A     = AA + DT*triform1(p,p1,t,t1,UU0(1:N),UU0(N+1:2*N));
   RSIDE = [[MM*UU0(1:N) + DT*FF(1:N);
            MM*UU0(N+1:2*N) + DT*FF(N+1:2*N)] + DT*CC*P];
   J = RD(1,:); RSIDE = RSIDE - A(:,J)*RD(2,:).'; 
   RSIDE(J) = RD(2,:).';
   A(J,:) = 0; A(:,J) = 0;
   AUX = zeros(size(A,1),1); AUX(RD(1,:)) = 1;
   A = spdiags(diag(A)+AUX,0,A);
   UU1 = A\RSIDE;
   RSIDEP = CC.'*UU1;
   J = RDP(1); RSIDEP = RSIDEP - AP(:,J)*RDP(2); 
   RSIDEP(J) = RDP(2);
   AP(J,:) = 0; AP(:,J) = 0;
   AUX = zeros(size(AP,1),1); AUX(RDP(1)) = 1;
   AP  = spdiags(diag(AP)+AUX,0,AP);
   Q   = AP\RSIDEP;
   P   = P - Q;
   UU1 = UU1 - DT*INVMM*CC*Q; % slightly better
  %  UU1 = UU1 - DT*CC*Q;
   DIFFDIV = max(abs(CC.'*UU1))
   DIFF    = max(abs(UU1 - UU0));
   I_DIFF  = [I,DIFF]
   UU0 = UU1; 
end
U = UU0(1:N); V = UU0(N+1:2*N);  

save daten5fb U V P 
disp(' Call bild05 and select demo5f ! ')
