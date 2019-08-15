function demo6
% Steady convection problem
% Elliptic system for convection following Stevens
% Int. J. Numer. Meth. in Fluids, 2 (1982), 349-366
% Simple iteration
% Example: Exact example in a unit square
% X,Y : Coordinates
% V   : Velocity at the center of triangle,
% Z/W :  Stream function/vorticity
% T   : Temperature
% SEGNR : ordered segment numbers for boundary
% KK, MM    : Steifigkeitsmatrix/Massenmatrix

clc, clear, format short, format compact
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp05'; FF2 = 'bsp05g'; FF3 = 'bsp06h';
% -- Parameter -------------------
MAXITER = 40;
NU     = 0.05;    % coeff. of viscosity [m*m/s], Ra = 1.0E3
LAMBDA  = NU;      % thermal conductivity
BETA   = 0.21E-3; % volume expansion coeff. [1/K]
KAPPA  = 0.04;    % heat transfer coeff. [J/(m*s*K)]
g      = 9.81;    % gravitational acceleration [m/(s*s)]
T_AIR  = 15;      % outside temp. [K] or [deg]
T0     = 0;      % init. temp. [K] or [deg]
SEGNR  = [1,2,3,4]; % Segmentnrn. des Rand
EPSILON = 0;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Im dim.-losen System ist NU = 1, LAMBDA = 1/PR, g*BETA = RA/PR
RA = 1.0E4; PR = 1;
NU = 1; LAMBDA = 1/PR; BETA = RA/(g*PR);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Parmeter = [NU,LAMBDA,BETA,KAPPA,T_AIR,g];
% -------------------------------------------
Start = 100; KK = [0,1];
while ~ismember(Start,KK)
   Start = input(' Initialization yes/no ? (1/0) ');
end
if Start == 1
   REFINE = 4;    % Number of uniform refinements
                  % REFINE = 5 gleiches Ergebnis
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
   IP = find(AUX == 1); % innere Punkte
   % -- data for inhomogeneous problem  -----
   % DATA = [z;w;t];
   DATA = bsp07_data(p);
   Z_EXACT = DATA(1,:)'; W_EXACT = DATA(2,:)'; T_EXACT = PR*DATA(3,:)'/RA;
   DELTA_T = PR*DATA(4,:)'/RA;  CONVEC_T = PR*DATA(5,:)'/RA;
   [RDZ,RCZ,RDW,RDT,RCT] = feval(FF3,p,e,t,T_EXACT);
   save daten6a p e t RAND IP T_EXACT Z_EXACT W_EXACT
   save daten6b RDZ RCZ RDW RDT RCT DELTA_T CONVEC_T
end
if Start == 1
   N = size(p,2); LIP = length(IP);
   W  = zeros(N,1); Z = W; Z(RDZ(1,:)) = RDZ(2,:)';
   T = T0*ones(N,1);
else
   load daten6a p e t RAND IP T_EXACT
   load daten6b RDZ RCZ RDW RDT RCT DELTA_T CONVEC_T
   load daten6c V W Z T
   N = size(p,2); LIP = length(IP);
end
% ------------------------------
N = size(p,2); LIP = length(IP);
% -------------------------------------------
[KK,MM] = matrizen(p,e,t);
% -- Die Matrix des linearen Systems -----
AA1 = [MM,          -KK(:,IP),        zeros(N,N);
      NU*KK(IP,:), EPSILON*MM(IP,IP),zeros(LIP,N);
      zeros(N,N),  zeros(N,LIP),     LAMBDA*KK];
AA1 = sparse(AA1);
W = zeros(N,1); Z = zeros(N,1); Z(RDZ(1,:)) = RDZ(2,:)';
T = T0*ones(N,1);
for ITER = 1:MAXITER
   AA = AA1;
   WCONVEC = trilin(p,t,W,Z);
   TCONVEC = trilin(p,t,T,Z);
   [WB,ZB,TB] = rightsides(p,t,T,RCZ,RCT,Parmeter);
   RSW = KK(:,RDZ(1,:))*Z(RDZ(1,:)) - ZB;
   RSZ = WB(IP)   - WCONVEC(IP);
   RST = LAMBDA*TB - TCONVEC + MM*(- LAMBDA*DELTA_T + CONVEC_T);
   % -- Gesamte Rechte Seite -----------------------
   RS = [RSW;RSZ;RST];
   % -- Dirichlet boundary condition for W --------
   if ~isempty(RDW)
      J = RDW(1,:); RS = RS - AA(:,J)*RDW(2,:)';
      RS(J) = RDW(2,:)';
      AA(J,:) = 0; AA(:,J) = 0;
      for I = 1:size(RDW,2), J = RDW(1,I); AA(J,J)  = 1; end
   end
   % -- Dirichlet boundary condition for T --------
   J = RDT(1,:); RS = RS - AA(:,N+LIP+J)*RDT(2,:)';
   RS(N+LIP+J) = RDT(2,:)';
   AA(N+LIP+J,:) = 0; AA(:,N+LIP+J) = 0;
   for I = 1:size(RDT,2), J = RDT(1,I); AA(N+LIP+J,N+LIP+J)  = 1; end
   % --LGS ---------------------------------------
   XX = AA\RS;
   % Sortieren W,Z,T ----------------
   WN     = XX(1:N); ZN = zeros(N,1);
   ZN(IP) = XX(N+1:N+length(IP)); ZN(RDZ(1,:)) = RDZ(2,:)';
   TN     = XX(N+LIP+1:N+LIP+N);
   DIFFZ  = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   DIFFT  = max(abs(TN - T));
   W = WN; Z = ZN; T = TN;
   ITER_DIFFZ_DIFFW_DIFFT = [ITER, DIFFZ, DIFFW, DIFFT]
end
V = velocity(p,e,t,Z);
save daten6c V W Z T
disp(' bild06 Aufrufen!')
%bild06
