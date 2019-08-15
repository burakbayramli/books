function demo11
% Navier-Stokes problem, Stream function vorticity method
% Coupled system following, linear triangular elements
% Barragy-Carey: Comm. Num. Meth. Eng. 9 (1993), 387-395
% simple Newton method
% Example: lid driven cavity
% X,Y   : Coordinates
% V     : Velocity at the center of triangle,
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% RDZ   : Dirichlet-RB fuer Z
% RCZ   : Cauchy-RB fuer Z
% RDW   : Dirichlet-RB fuer W
% FF1   : File for first mesh (entfaellt bei MATLAB TOOLBOX)
% FF2   : File for geometry in MATLAB Format 
% FF3   : File for boundary conditions   

clc, clear, format short, format compact
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp07h';
% -- Parameters -------------------
MAXITER = 10;
%Continue w.r.t. NU: NU = 0.01,0.005,0.004,0.003,0.002,0.001
NU     = 0.01; % coeff. of viscosity [m*m/sec]
VS     = 1;    % slip-boundary data
SEGNR = [1,2,3,4]; % Segmentnrn. des Randes
EPSILON = 0;
Parmeter = [0,NU,VS];
Start = 100; KK = [0,1];
while ~ismember(Start,KK)
   Start = input(' Initialization yes/no ? (1/0) ');
end
if Start == 1
   OPTION = 100;
   while ~ismember(OPTION,[1,2])
      OPTION = input(' Which mesh generation? (1/2)');
   end
   switch OPTION
   case 1, disp(' mesh without TOOLBOX, slow ')
      REFINE = 5;    %8 Number of uniform refinements
      [p,e,t] = feval(FF1); % erstes Gitter
      for J = 1:REFINE
         disp(' Refinemesh ')
         [p,e,t] = mesh01_t(FF2,p,e,t);
     %   p       = mesh10(p,e,t,4); % Jigglemesh
     %   t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
      end
     RAND = e;
     % -- Innere Punkte --------------
     LP = size(p,2); AUX = zeros(1,LP);
     for I = 1:LP
        if isempty(find(e(1,:) == I)), AUX(I) = 1; end
     end
     IP = find(AUX == 1);
   case 2, disp(' mesh with TOOLBOX, fast ')
      REFINE = 5;
      [p,e,t,RAND,IP] = prepar(FF2,REFINE,SEGNR);
   end
   %load daten p e t IP RAND
   [RDZ,RCZ,RDW] = feval(FF3,p,e,t,Parmeter);
   % -- initial values -----------------
   N = size(p,2); M = length(IP); Z = zeros(N,1); W = zeros(N,1);
   % -- Matrix und rechte Seite --------
   [KK,MM,Q] = matrizen1(p,e,t,RCZ);
   save daten7a p e t IP Parmeter
   save daten7b RDZ RCZ RDW KK MM Q
else
   load daten7a p e t IP Parmeter
   Parmeter = [0,NU,VS];
   save daten7a p e t IP Parmeter
   load daten7b RDZ RCZ RDW KK MM Q
   load daten7c W Z
   N = size(p,2); M = length(IP);
end
AA = [MM, -KK(:,IP); NU*KK(IP,:), EPSILON*MM(IP,IP)];
AA = sparse(AA);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for ITER = 1:MAXITER
   [WR,T_W,T_Z] = trilin1(p,t,W,Z);
   RS = [KK(:,RDZ(1,:))*Z(RDZ(1,:)) - Q; - WR(IP)];
   RS = RS - AA*[W;Z(IP)];
   BB = AA + [zeros(N,N+M); T_W(IP,:), T_Z(IP,IP)];
   BB = sparse(BB);
   XX = BB\RS;
   WN = W + XX(1:N); ZN = zeros(N,1); ZN(IP) = Z(IP) + XX(N+1:N+length(IP));
   ZN(RDZ(1,:)) = RDZ(2,:);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER,DIFFZ, DIFFW]
end
%V = velocity(p,e,t,Z);
save daten7c W Z
disp(' bild07 Aufrufen!')
%bild07
