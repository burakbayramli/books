function demo4
% E.W. Gekeler, Release 07.12.09 
% Example: Thermal Flow in a cup
% Taylor-Hood elements with convection term
% linear buoyancy term 
% DAE problem with semi-explicit Runge-Kutta method
% X,Y : coordinates
% U,V :  velocity in x- and y-direction
% TT  : temperature
% P   :  Pressure
% p,e,t: nodes,edges,triangles
% p1,t1: data of midpoints and their indices in triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]
% FF1 : File for first mesh (not used in MATLAB TOOLBOX)
% FF2 : File for geometry in MATLAB format 
% FF3 : File for boundary conditions and loads  
% OPTION_MESH = 1/2 Mesh without/with MATLAB TOOLBOX
% HERE WITH LUMPED MASS MATRIX

clear, clc, format short, format compact
% Example:
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h_1';
% Segment nrs. for outer boundary
segnr = [1,2,3,4];
% -- Data for iteration ---------------------
maxiter = 400;   % step number for time iteration,
DT      = 0.1;       % time step  
linear  = 0;     % linear 0/1 with/without convection term
OPTION_MESH = 2; REFINE = 2; % number of uniform mesh refinements
OPTION_MESH = 1; REFINE = 0; % number of uniform mesh refinements
%REFINE = 2;
% -- Parameters -------------------
nu      = 0.005;   % coeff. of viscosity
lambda  = nu;      % thermal conductivity [m*m/s]
g       = 9.81;    % grav. acceleration   [m/(s*s)]
beta    = 0.21E-3; % beta = Gr/g; volume expansion coeff. [1/K]
kappa   = 0.04;    % heat transfer coefficient [J/(m*m*K)
g_beta = g*beta;
t_air  = 15;      % outside temperature [K] or [deg]
epsilon = 0;         % Penalty Term, epsilon = 0.1 for stabilization
Parmeter = [nu,lambda,epsilon,g_beta];

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
   [p1,e,t1] = mesh06_t(p,e,t); % e is enlarged by a further row 8

   [S,C,D,MM,B] = taylor_hood(p,t,p1,t1);
   B = 100*B; % weighting factor
   N = size(p,2) + size(p1,2); NULL = zeros(N,N); N1 = size(p,2);
   % -- Dirichlet boundary conditions --------------------
   [RDU,RDV,RDT,RDP] = feval(FF3,p,e,p1);
   RD = [RDU(1,:),RDV(1,:) + N, RDT(1,:) + 2*N;
         RDU(2,:),RDV(2,:), RDT(2,:)];
   MM1 = diag(MM); clear MM 
   %a = Area/sum(MM1) factor after Zienkiewicz, p. 320
   MM2 = 0.633*spdiags(MM1,0,N,N); % for buoyancy
   MM3 = ones(3*N,1)./[MM1;MM1;MM1]; MM3(RD(1,:)) = 0; 
   INVM = spdiags(MM3,0,3*N,3*N); 
   MATRIXP = [C;D;zeros(N,N1)];
   MATRIXP1 = MATRIXP; MATRIXP1(RD(1,:)) = 0;
   GRADP = MATRIXP.'*INVM*MATRIXP1;
   J = RDP(1);
   GRADP(J,:) = 0; GRADP(:,J) = 0; GRADP(J,J) = 1; 
   INVM  = DT*INVM;
   save daten4a p e t p1 t1
   save daten4c S INVM GRADP MATRIXP RD MM2 RDP
end  
load daten4a p e t p1 t1
load daten4c S INVM GRADP MATRIXP RD MM2 RDP
N = size(p,2) + size(p1,2); 
% -- starting values --------------
if Start == 1 % cold start
   U = zeros(N,1); V = U; TT = t_air*ones(N,1);
else
   load daten4b U V TT
end
UU1 = [U;V;TT]; 
for I = 1:maxiter  
   UU1(RD(1,:)) = RD(2,:); 
   A = triform5(p,p1,t,t1,UU1,S,MM2,nu,lambda,g_beta,linear);
   RSIDEP = MATRIXP.'*(-UU1 + alfa21*INVM*A*UU1)/(DT*alfa21);
   RSIDEP(RDP(1)) = RDP(2);
   P1 = GRADP\RSIDEP; 
   FF1 = -A*UU1 + MATRIXP*P1; 
   UU2 = UU1 + alfa21*INVM*FF1;
   A = triform5(p,p1,t,t1,UU2,S,MM2,nu,lambda,g_beta,linear);
   RSIDEP = MATRIXP.'*(-UU1 - INVM*(alfa31*FF1 - alfa32*A*UU2))/(DT*alfa32); 
   RSIDEP(RDP(1)) = RDP(2);
   P2 = GRADP\RSIDEP; 
   FF2 = -A*UU2 + MATRIXP*P2;
   UU3 = UU1 + INVM*(alfa31*FF1 + alfa32*FF2);  
   A = triform5(p,p1,t,t1,UU3,S,MM2,nu,lambda,g_beta,linear);
   RSIDEP = MATRIXP.'*(-UU1 - INVM*(beta2*FF2 - beta3*A*UU3))/(DT*beta3);   
   RSIDEP(RDP(1)) = RDP(2);
   P3 = GRADP\RSIDEP; 
   FF3 = -A*UU3 + MATRIXP*P3;
   UU4 = UU1 + INVM*(beta2*FF2 + beta3*FF3);    
   DIFF = max(abs(UU4-UU1)); ITER_DIFF = [I,DIFF]
   UU1 = UU4; 
   if DIFF > 300, disp(' reduce time step! '), return, end
end
U = UU1(1:N); V = UU1(N+1:2*N); TT = UU1(2*N+1:3*N); P = P3;
save daten4b U V TT P 
disp(' Call bild01 and select demo4 ! ')
