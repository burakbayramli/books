function demo4
% Example: Thermal flow in a cup
% Steady unscaled convection problem
% Elliptic system for convection following Stevens
% Int. J. Numer. Meth. in Fluids, 2 (1982), 349-366
% Simple iteration
% X,Y     : Coordinates
% V(1:2,:): Velocity at the center of triangle,
% Z/W     :  Stream function/vorticity
% T       : Temperature
% SEGNR   : ordered segment numbers for boundary
% KK, MM  : Steifigkeitsmatrix/Massenmatrix

clc, clear, format short, format compact
%%%%%%% DATENEINGEBE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp01'; FF2 = 'bsp04h';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
MAXITER = 10;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Parameter -------------------
NU     = 0.05;    % coeff. of viscosity [m*m/s], Ra = 1.0E3
NU     = 0.016;    % coeff. of viscosity [m*m/s], Ra = 1.0E4
% simple It. fails for NU = 0.005
%NU     = 0.005;    % coeff. of viscosity [m*m/s], Ra = 1.0E5
LAMBDA  = NU;      % thermal conductivity
BETA   = 0.21E-3; % volume expansion coeff. [1/K]
KAPPA  = 0.04;    % heat transfer coeff. [J/(m*s*K)]
g      = 9.81;    % gravitational acceleration [m/(s*s)]
T_AIR  = 15;      % outside temp. [K] or [deg]
T0     = 15;      % init. temp. [K] or [deg]
SEGNR  = [1,2,3,4];
EPSILON = 0;
%%% ENDE DER DATENEINGABE %%%%%%%%%%%%%%%%
Parmeter = [NU,LAMBDA,BETA,KAPPA,T_AIR,g];

[p,e,t] = feval(FF1); RAND = e; N = size(p,2);
% Berechne innere Punkte IP
% Es muss gelten {IP,RDZ(1,:)} = [1:N]!!!
AUX = zeros(1,N);
for I = 1:N
   if isempty(find(e(1,:) == I)), AUX(I) = 1; end
end
IP = find(AUX == 1); LIP = length(IP);
[RDZ,RCZ,RDW,RDT,RCT] = feval(FF2,p,e,t); %Randwerte
[KK,MM] = matrizen(p,e,t);
W = zeros(N,1);
Z = zeros(N,1); Z(RDZ(1,:)) = RDZ(2,:)';
T = T0*ones(N,1);
save daten4a p e t RAND IP Parmeter
save daten4b RDZ RCZ RDW RDT RCT
% -- Die Matrix des linearen Systems -----
AA1 = [MM,          -KK(:,IP),        zeros(N,N);
      NU*KK(IP,:), EPSILON*MM(IP,IP),zeros(LIP,N);
      zeros(N,N),  zeros(N,LIP),     LAMBDA*KK];
AA1 = sparse(AA1);
for ITER = 1:MAXITER
   AA = AA1;
   WCONVEC = trilin(p,t,W,Z);
   TCONVEC = trilin(p,t,T,Z);
   [WB,ZB,TB] = rightsides(p,t,T,RCZ,RCT,Parmeter);
   RSW = KK(:,RDZ(1,:))*Z(RDZ(1,:)) - ZB;
   RSZ = WB(IP)   - WCONVEC(IP);
   RST = LAMBDA*TB - TCONVEC;
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
save daten4c V W Z T
disp(' bild04 Aufrufen!')
%bild04
