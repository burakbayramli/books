function demo5
% Example: Convection in a unit square
% Steady convection problem
% Elliptic system for convection following Stevens
% Int. J. Numer. Meth. in Fluids, 2 (1982), 349-366
% Simple NEWTON iteration
% HIER MANUELLE FORTSETZUNG FUER RAYLEIGH-ZAHL (RESTART = 0)
% 1E3, 1E4, 2E4, 4E4, 6E4, 7E4, 7.5E4, 8E4 , 8.2E4, 8.4E4  maximal
% X,Y      : Coordinates
% V        : Velocity at the center of triangle,
% Z/W      : Stream function/vorticity, T: Temperature
% SEGNR    : ordered segment numbers for boundary
% KK,MM,CC : Steifigkeitsmatrix,Massenmatrix,Matrix fuer T_X
% -- Parameter -------------------
% NU       : coeff. of viscosity; LAMBDA: thermal conductivity
% BETA     : volume expansion coeff.;
% KAPPA    : heat transfer coeff. (nur bei Cauchy-RB fuer temp.)
% g        : gravitational acceleration;
% T_AIR    : outside temperature
% RA: Rayleigh number; PR: Prandtl number; GR: Grashoff number
% Im dim.-losen System ist NU = 1, LAMBDA = 1/PR, g*BETA = Gr = RA/PR

clc, clear, format short, format compact
%%%%%%%%% DATENEINGABE %%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp05'; FF2 = 'bsp05g'; FF3 = 'bsp05h';
MAXITER = 30;        % Anzahl It.-schritte
SEGNR   = [1,2,3,4]; % Segmentnrn. des Rand
EPSILON = 0;      % Penalty value
%%%HIER RAYLEIGH-ZAHL AENDERN FUER FORTSETZUNG %%%%%%
Ra = 1.0E3; Pr = 1; KAPPA = 0; T_AIR = 0;
NU = 1; LAMBDA = 1/Pr; g = 9.81; BETA = Ra/(g*Pr);
%%% ENDE DER DATENEINGABE %%%%%%%%%%%%%%%%%%%%%%%%%%%%
Parmeter = [NU,LAMBDA,BETA,KAPPA,T_AIR,g];
Parmeter1 = [Pr,Ra];
 Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' Cold start or restart? (1/0) ');
end
if Start == 1
   REFINE = 5;    % Number of uniform refinements
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
   [RDZ,RCZ,RDW,RDT,RCT] = feval(FF3,p,e,t);
   N = size(p,2); LIP = length(IP);
   [KK,MM,CC] = matrizen(p,e,t);
% -- Die Matrix des linearen Systems -----
   AA1 = [MM,          - KK(:,IP),          zeros(N,N);
          NU*KK(IP,:),   EPSILON*MM(IP,IP), zeros(LIP,N);
          zeros(N,N),    zeros(N,LIP),      LAMBDA*KK];
   AA1 = sparse(AA1);
   save daten5a p e t RAND IP
   save daten5b RDZ RCZ RDW RDT RCT AA1 KK CC MM
end
if Start == 1
   N = size(p,2); LIP = length(IP);
   W  = zeros(N,1); Z = W; T = W;
   if ~isempty(RDW), W(RDW(1,:)) = RDW(2,:)'; end
   if ~isempty(RDZ), Z(RDZ(1,:)) = RDZ(2,:)'; end
   if ~isempty(RDT), T(RDT(1,:)) = RDT(2,:)'; end
else
   load daten5a p e t RAND IP
   load daten5b RDZ RCZ RDW RDT RCT AA1 KK CC MM
   load daten5c W Z T;
   N = size(p,2); LIP = length(IP);
   % HIER MANUELLE FORTSETZUNG FUER RAYLEIGH-ZAHL
   %1E4, 2E4, 4E4, 6E4, 7E4, 7.5E4, 8E4 , 8.2E4, 8.4E4  maximal
   Ra = 1E4; BETA = Ra/(g*Pr);
   EPSILON = 0;
   Parmeter = [NU,LAMBDA,BETA,KAPPA,T_AIR,g];
   Parmeter1 = [Pr,Ra]
end
% -- Ev. EPSILON ABAENDERN -----
N = size(p,2); LIP = length(IP);
AA1(N+1:N+LIP,N+1:N+LIP) = EPSILON*MM(IP,IP);
for ITER = 1:MAXITER
   AA = AA1;
   WCONVEC = trilin(p,t,W,Z);
   TCONVEC = trilin(p,t,T,Z);
   [T_W,T_Z,T_T] = trilingrad1(p,t,W,Z,T);
   [ZB,TB] = cauchybc(p,t,T,RCZ,RCT,Parmeter);
   AA(N+1:N+LIP,N+LIP+1:N+LIP+N) = -g*BETA*CC(IP,:);
   % -- Rechte Seite des Systems -----------------------
   RSW = KK(:,RDZ(1,:))*RDZ(2,:)' - ZB;
   RSZ = - WCONVEC(IP);
   RST = - TCONVEC + LAMBDA*TB;
   RS = [RSW;RSZ;RST];
   % -- Addition zu AA fuer Gradienten ---------------
   BB = [zeros(N,2*N+LIP);
         T_W(IP,:), T_Z(IP,IP),zeros(LIP,N);
         zeros(N,N),Pr*T_T(:,IP), Pr*T_W];
   BB = sparse(BB);
   GG = AA + BB; GG = sparse(GG); RSN = -(AA*[W;Z(IP);T] - RS);
   % -- Dirichlet boundary condition for W --------
   if ~isempty(RDW)
      J = RDW(1,:); RSN(J) = 0; GG(J,:) = 0; GG(:,J) = 0;
      for I = 1:size(RDW,2), J = RDW(1,I); GG(J,J)  = 1; end
   end
   % -- Dirichlet boundary condition for T --------
   J = RDT(1,:); RSN(N+LIP+J) = 0; GG(N+LIP+J,:) = 0; GG(:,N+LIP+J) = 0;
   for I = 1:size(RDT,2), J = RDT(1,I); GG(N+LIP+J,N+LIP+J)  = 1; end
   XX = GG\RSN;
   WN = W + XX(1:N);
   ZN = zeros(N,1); ZN(IP) = Z(IP)+XX(N+1:N+LIP); ZN(RDZ(1,:)) = RDZ(2,:)';
   TN = T + XX(N+LIP+1:N+LIP+N);
   DIFFW = max(abs(WN-W)); DIFFZ = max(abs(ZN-Z)); DIFFT = max(abs(TN-T));
   W = WN; Z = ZN; T = TN;
   ITER_DIFFW_DIFFZ_DIFFT = [ITER, DIFFW, DIFFZ, DIFFT]
end
SAVE = 100;
while ~ismember(SAVE,[0,1])
   SAVE = input(' Daten Speichern bei Konvergenz? yes/no ? (1/0) ');
end
if SAVE == 1
   save daten5c W Z T
end
P = pressure_t(p,e,t,T,Z,Parmeter1);
save daten5c W Z T P Parmeter1
disp(' bild05 Aufrufen!')
