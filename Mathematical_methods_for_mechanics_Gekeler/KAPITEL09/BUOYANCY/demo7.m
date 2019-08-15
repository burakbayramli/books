function demo7
% E.W. Gekeler, Release 07.12.09 
% Example: Benard cell
% as demo7a.m but without Dirichlet pressure condition
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
FF1 = 'bsp03'; FF2 = 'bsp03g'; FF3 = 'bsp03h';
% Segment nrs. of outer boundary :
segnr_a  = [37,34,31,28,25,22,19,16,13,10,7,3]; %above
segnr_b = 4;                                    %left 
segnr_c = [1,5,8,11,14,17,20,23,26,29,32,35];   %below
segnr_d = 36;                                   % right
segnr = [segnr_a,segnr_b,segnr_c,segnr_d];

% -- Data for iteration ----------------
maxiter = 200;   % step number for time iteration % total 1000
linear  = 0;     % linear 0/1 with/without convection term
OPTION_MESH = 1; REFINE = 3; % Number of uniform mesh refinements
OPTION_MESH = 2; REFINE = 2; % Number of uniform mesh refinements

% -- Parameters ---------------
scaled = 1;

switch scaled
case 0, % unscaled problem for water
   nu      = 1.14E-6;      % coeff. of viscosity
   lambda  = 1.4E-7;  % thermal conductivity [m*m/s]
   g       = 9.81;   % grav. acceleration
   beta    = 0.21E-3;
   Ra      = 2.025E4; %Rayleigh number,    Pr = nu/lambda
   g_beta  = g*beta;  % 
   kappa   = 0.59;    % heat transfer coeff. [J/(m*s*K)]
   DT      = 0.1;        % time step
   TU      = 1; TO = 0;
   scale_factor = 0.01;
case 1, % scaled problem for water Sec. 0908 (b) natural convection
   nu      = 1;      
   lambda  = 1/7;  % = 1/Pr , Pr = 7.171
   g_beta  = 2.5E3;   % = Fr = Ra^2/Pr, see Ninomiya
  % kappa   = 0.59;    % heat transfer coeff. [J/(m*s*K)]
   DT = 0.002;        % time step
   TU = 1; TO = 0;
   scale_factor = 1;
   
case 2, % working example
   nu      = 1;      % coeff. of viscosity
   lambda  = 0.075;  % thermal conductivity [m*m/s]
   g       = 9.81;   % grav. acceleration
   beta    = 142;
   g_beta  = g*beta;
   kappa   = 0.04;    % heat transfer coeff. [J/(m*s*K)]
   DT = 0.002;        % time step
   TU = 1; TO = 0;
   scale_factor = 1;
end
parmeter_h = [TU,TO,segnr,scaled];

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
   p = p*scale_factor;
   %bild00(p,e,t), pause
   [p1,e,t1] = mesh06_t(p,e,t); % e is enlarged by a further row 8

   [S,C,D,MM,B] = taylor_hood(p,t,p1,t1);
   B = 100*B; % weighting factor
   N = size(p,2) + size(p1,2); NULL = zeros(N,N); N1 = size(p,2);
   % -- Dirichlet boundary conditions --------------------
   [RDU,RDV,RDT] = feval(FF3,p,e,p1,parmeter_h);
   RD = [RDU(1,:),RDV(1,:) + N, RDT(1,:) + 2*N;
         RDU(2,:),RDV(2,:), RDT(2,:)];
   MM1 = diag(MM); clear MM 
   %a = Area/sum(MM1) factor after Zienkiewicz, p. 320
   MM2 = 1.6*spdiags(MM1,0,N,N); % for buoyancy
   MM3 = ones(3*N,1)./[MM1;MM1;MM1]; MM3(RD(1,:)) = 0; 
   INVM = spdiags(MM3,0,3*N,3*N); 
   MATRIXP  = [C;D;zeros(N,N1)];
   %AUX = MATRIXP.'*INVM;
   GRADP = MATRIXP.'*INVM*MATRIXP; 
   GRADP = [GRADP, B]; 
   GRADP = [GRADP;[B.',0]];   
   INVM  = DT*INVM;
   save daten7a p e t p1 t1 parmeter_h
   save daten7c S INVM GRADP MATRIXP RD MM2
end  
load daten7a p e t p1 t1 parmeter_h
load daten7c S INVM GRADP MATRIXP RD MM2
N = size(p,2) + size(p1,2); 
% -- starting values --------------
if Start == 1 % cold start
   U = zeros(N,1); V = U; TT = TO*ones(N,1);
else
   load daten7b U V TT
end
UU1 = [U;V;TT]; 
for I = 1:maxiter  
   UU1(RD(1,:)) = RD(2,:); 
   A = triform5(p,p1,t,t1,UU1,S,MM2,nu,lambda,g_beta,linear);
   RSIDEP = [MATRIXP.'*(-UU1 + alfa21*INVM*A*UU1);0]/(DT*alfa21);
   P1 = GRADP\RSIDEP; P1 = P1(1:end-1);
   FF1 = -A*UU1 + MATRIXP*P1; 
   UU2 = UU1 + alfa21*INVM*FF1;
   A = triform5(p,p1,t,t1,UU2,S,MM2,nu,lambda,g_beta,linear);
   RSIDEP = [MATRIXP.'*(-UU1 - INVM*(alfa31*FF1 - alfa32*A*UU2));0]/(DT*alfa32); 
   P2 = GRADP\RSIDEP; P2 = P2(1:end-1);
   FF2 = -A*UU2 + MATRIXP*P2;
   UU3 = UU1 + INVM*(alfa31*FF1 + alfa32*FF2);  
   A = triform5(p,p1,t,t1,UU3,S,MM2,nu,lambda,g_beta,linear);
   RSIDEP = [MATRIXP.'*(-UU1 - INVM*(beta2*FF2 - beta3*A*UU3)); 0]/(DT*beta3);   
   P3 = GRADP\RSIDEP; P3 = P3(1:end-1);
   FF3 = -A*UU3 + MATRIXP*P3;
   UU4 = UU1 + INVM*(beta2*FF2 + beta3*FF3);    
   DIFF = max(abs(UU4-UU1)); ITER_DIFF = [I,DIFF]
   UU1 = UU4; 
   if DIFF > 300, disp(' reduce time step! '), return, end
end
U = UU1(1:N); V = UU1(N+1:2*N); TT = UU1(2*N+1:3*N); P = P3;
save daten7b U V TT P 
disp(' Call bild03 and select demo7 ! ')
