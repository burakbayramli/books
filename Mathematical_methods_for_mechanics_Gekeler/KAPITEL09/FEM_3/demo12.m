function demo12
% same as DEMO11.M but linear parallelogram elements 
% Navier-Stokes problem, Stream function vorticity method
% Coupled system following
% Barragy-Carey: Comm. Num. Meth. Eng. 9 (1993), 387-395
% simple Newton method
% Example: lid driven cavity
% X,Y   : Coordinates
% V     : Velocity at the center of triangle,
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% RDZ   : Dirichlet-RB for Z
% RCZ   : Cauchy-RB for Z
% FF1   : File for first mesh 
% FF3   : File for boundary conditions  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Choose first ''New start'' with NU = 0.01
% then ''restart'' with NU = 0.005,NU = 0.004, NU = 0.003,
% NU = 0.002, NU = 0.0015, NU = 0.001
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5 
clc, clear, format short, format compact
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp09'; FF3 = 'bsp07h'; % FF3 same as in DEMO11.M
% -- Parameters -------------------
NU  = 0.01; % coeff. of viscosity [m*m/sec]

MAXITER = 8;    % No. of Newton steps
REFINE = 4;     % 4  Number of uniform refinements
SEGNR = [1,2,3,4]; % Segmentns. of boundary
VS     = 1;     % slip-boundary data
EPSILON = 0;
Parmeter = [0,NU,VS];
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or restart ? (1/0) ');
end
%Start = 1;
if Start == 1
   [p,e,q] = feval(FF1); % erstes Gitter
   for J = 1:REFINE, disp(' Refinemesh ')
      [p,e,q] = mesh01_q([],p,e,q);
     % [p,e,t1,q] = mesh01_tq([],p,e,[],q);

   end
%   clf, mesh36(p,q,'y'),
%   pause
   % -- Compute Nos. of interior points IP ---
   LP = size(p,2); AUX = zeros(1,LP);
   for I = 1:LP
      if isempty(find(e(1,:) == I)), AUX(I) = 1; end
   end
   IP = find(AUX == 1);

   [RDZ,RCZ,RDW] = feval(FF3,p,e,q,Parmeter);
   % -- initial values -----------------
   N = size(p,2); M = length(IP); Z = zeros(N,1); W = zeros(N,1);
   % -- Matrix and right side of linear problem --
   [KK,MM,Q] = matrizen2(p,e,q,RCZ);
   save daten12a p e q IP Parmeter
   save daten12b RDZ RCZ RDW KK MM Q
else
   Parmeter = [0,NU,VS];
   load daten12a p e q IP Parmeter
   load daten12b RDZ RCZ RDW KK MM Q
   load daten12c W Z
   N = size(p,2); M = length(IP);
end
AA = [MM, -KK(:,IP); NU*KK(IP,:), EPSILON*MM(IP,IP)];
AA = sparse(AA); % this is matrix of linear problem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for ITER = 1:MAXITER
   [WR,T_W,T_Z] = trilin2(p,q,W,Z); % convective terms and gradients
   RS = [KK(:,RDZ(1,:))*Z(RDZ(1,:)) - Q; - T_W(IP,:)*W];
   RS = RS - AA*[W;Z(IP)];
   BB = AA + [zeros(N,N+M); T_W(IP,:), T_Z(IP,IP)];
   BB = sparse(BB);
   XX = BB\RS;  % Solve linear sytem of Newton step
   WN = W + XX(1:N); ZN = zeros(N,1); ZN(IP) = Z(IP) + XX(N+1:N+length(IP));
   ZN(RDZ(1,:)) = RDZ(2,:);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER,DIFFZ, DIFFW]
end
save daten12c W Z
disp(' Call ''bild14''!')
%bild14



 






