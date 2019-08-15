function demo2
% Example: convection in a closed compartment
% Unsteady unscaled convection
% X,Y : Coordinates
% V(1:2,:): Velocity at the center of triangle,
% Z/W : Stream function/vorticity
% T   : Temperature
% SEGNR  : ordered segment numbers for boundary

clc, clear, format compact, format short e
problem = 2;
switch problem
case 1 % Daten fuer unskaliertes Problem
   % -- Example: Geometry file and file of boundary values ---
 %  FF1 = 'bsp02a'; FF2 = 'bsp02ga'; FF3 = 'bsp02h2a'; % W_RD = 0
   FF1 = 'bsp02a'; FF2 = 'bsp02ga'; FF3 = 'bsp02h1a'; % W_RD = W_WALL
   MAXITER = 2100;  % 7 Minuten
   DT     = 0.2;     % time step [s]
   % -- Parameters -------------------
   NU     = 1.8E-6;  % coeff. of viscosity [m*m/s] at Ra = 1.89E8
   LAMBDA = 1.35E-7; % thermal diffusivity [m*m/s] at Ra = 1.89E8
   BETA   = 4.6E-4;  % volume expansion coeff. [1/K];
   g      = 9.81;    % gravitational acceleration (earth)
   KAPPA  = 0;       % transfer coeff. [J/(m*s*K)] (not used)
   T_AIR   = 0;      % outside temp. [K] or [deg] (not used)
   T0     = 21.5;    % init. temp. [K] or [deg]
   REFINE = 4;       % Number of uniform refinements
   SEGNR  = [1,2,3,4];
   XLAENGE = 0.1; YLAENGE = 0.15;
case 2 % Daten fuer skaliertes Problem
   % -- Example: Geometry file and file of boundary values ---
   FF1 = 'bsp02b'; FF2 = 'bsp02gb'; FF3 = 'bsp02h2b';
  % FF1 = 'bsp02b'; FF2 = 'bsp02gb'; FF3 = 'bsp02h1b';
   MAXITER = 2100;  % 7 Minuten
   
   MAXITER = 10;
   DT     = 0.2;     % time step [s]
   % -- Parameters -------------------
   NU     = 1.8E-6;  % coeff. of viscosity [m*m/s] at Ra = 1.89E8
   LAMBDA = 1.35E-7; % thermal diffusivity [m*m/s] at Ra = 1.89E8
   BETA   = 4.6E-4;  % volume expansion coeff. [1/K];
   g      = 9.81;    % gravitational acceleration
   KAPPA  = 0;       % transfer coeff. [J/(m*s*K)] (not used)
   T_AIR   = 0;      % outside temp. [K] or [deg] (not used)
   T0     = 0;       % init. temp. [K] or [deg]
   REFINE = 4;       % Number of uniform refinements
   SEGNR  = [1,2,3,4];
   XLAENGE = 1; YLAENGE = 1.5;
   DELTA_T = 3; DELTA_L = 0.15; 
   PR = NU/LAMBDA;
   RA = g*BETA*DELTA_T*DELTA_L^3/(NU*LAMBDA);
   %% Im dim.-losen System ist
   % t := t*NU_ALT/DELTA_L^2, NU = 1, LAMBDA = 1/PR, g*BETA = RA/PR
   % also
   DT = DT*NU/DELTA_L^2;
   NU = 1; LAMBDA = 1/PR; BETA = RA/(g*PR); 
end
Parmeter = [DT,NU,LAMBDA,BETA,KAPPA,T_AIR];
Start = 100; KK = [0,1];
while ~ismember(Start,KK)
   Start = input(' Initialization yes/no ? (1/0) ');
end
%Start = 1;
if Start == 1
     flag = 2; % Gittererzeugung ohne/mit TOOLBOX
     switch flag
     case 1  
        [p,e,t] = feval(FF1); % erstes Gitter
        for J = 1:REFINE
            disp(' Refinemesh ')
            [p,e,t] = mesh01_t(FF2,p,e,t);
         %   p       = mesh10(p,e,t,4); % Jigglemesh
         %   t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
         end
         RAND = e;
      case 2
         [p,e,t,RAND] = prepar2(REFINE,FF2,XLAENGE,YLAENGE);
      end
   % -- Computation of offset data
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   % -- boundary data ------------------------
   [RDZ,RCZ,RDW,RDT,RCT] = feval(FF3,p,e,t,Parmeter);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   save daten2a p e t RAND Parmeter
   save daten2b RDZ RCZ RDW RDT RCT NACHBAR NORMALEN
end
load daten2a p e t RAND Parmeter
load daten2b RDZ RCZ RDW RDT RCT NACHBAR NORMALEN
Parmeter = [DT,NU,LAMBDA,BETA,KAPPA,T_AIR];

% ------------------------------
if Start == 1
   W  = zeros(size(p,2),1); Z = W;
   T = T0*ones(size(p,2),1);
else, load daten2c V W Z T; end
tic
for ITER = 1:MAXITER
   ZN  = ellipt1(p,t,RDZ,RCZ,W);
   WBA = wbound(p,e,RAND,t,RDW,W,Z,NACHBAR);
   WN  = vorticity_k(p,t,WBA,W,ZN,T,Parmeter);
   TN  = convection(p,t,ZN,RDT,RCT,T,Parmeter);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   DIFFT = max(abs(TN - T));
   Z = ZN; W = WN; T = TN;
   ITER
   DIFFZ_DIFFT_DIFFW = [DIFFZ,DIFFT,DIFFW]
end
V = velocity(p,e,t,Z);
save daten2c V W Z T
t = toc
disp(' bild02 Aufrufen!')
%bild02
