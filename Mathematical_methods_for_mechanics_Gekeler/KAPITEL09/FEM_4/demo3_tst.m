function demo3
% Convection problem, time-dependent, see H.Ninomiya/K.Onishi, p. 146
% Example: convection in a square box
% X,Y     : Coordinates
% V(1:2,:): Velocity at the center of triangle,
% Z/W     : Stream function/vorticity
% T       : Temperature
% SEGNR   : ordered segment numbers for boundary

clc, clear, format compact, format short 
% -- Example: Geometry file and file of boundary values ---
% Square 3 mal 3
%FF1 = 'bsp03a'; FF2 = 'bsp03ga'; FF3 = 'bsp03h1'; DT = 0.5; MAXITER = 200;
% unit square
FF1 = 'bsp03b'; FF2 = 'bsp03gb'; FF3 = 'bsp03h1'; DT = 0.5; MAXITER = 100;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
BETA   = 0.21E-3; % volume expansion coeff. [1/K];
KAPPA  = 0;       % transfer coeff. [J/(m*s*K)] (not used)
g      = 9.81;    % gravitational acceleration [m/(s*s)]
T_AIR   = 0;      % outside temp. [K] or [deg]  (not used)
%T0     = 5;      % init. temp. [K] or [deg]

% Temperatur rechts = 1, unit square, 
%% Im dim.-losen System ist NU = 1, LAMBDA = 1/PR, g*BETA = RA/PR
   Ra = 1.0E5; Pr = 1; NU = 1; LAMBDA = 1/Pr; BETA = Ra/(g*Pr);
   T_RECHTS = 1; MAXITER = 200; DT = 0.0001; T0 = 0.5; REFINE = 4;
SEGNR  = [1,2,3,4];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Parmeter = [DT,NU,LAMBDA,BETA,KAPPA,T_AIR,T_RECHTS];
Parmeter1 = [Pr,Ra];
Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' Cold start or restart? (1/0) ');
end
if Start == 1, OPTION = 2;
   [p,e,t,RAND,INNERPKTE] = start4stream(FF1,FF2,OPTION,REFINE,SEGNR); 
   if OPTION == 2
      XLAENGE = 1; YLAENGE = 1;
      [p,e,t,RAND] = prepar2a(p,e,t,FF2,XLAENGE,YLAENGE,SEGNR);
   end
%   bild00(p,e,t)
%   pause
   % -- Computation of offset data
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   % -- boundary data ------------------------
   [RDZ,RCZ,RDW,RDT,RCT] = feval(FF3,p,e,t,Parmeter);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   save daten3a p e t RAND Parmeter
   save daten3b RDZ RCZ RDW RDT RCT NACHBAR 
end
load daten3a p e t RAND Parmeter
load daten3b RDZ RCZ RDW RDT RCT NACHBAR 
% ------------------------------
if Start == 1
   W  = zeros(size(p,2),1); Z = W;
   T = T0*ones(size(p,2),1);
else, load daten3c W Z T; end
for ITER = 1:MAXITER
   ZN    = ellipt1(p,t,RDZ,RCZ,W);
   WBA  = wbound(p,e,RAND,t,RDW,W,ZN,NACHBAR);
   WN    = vorticity_k(p,t,WBA,W,ZN,T,Parmeter);
   TN   = convection(p,t,ZN,RDT,RCT,T,Parmeter);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   DIFFT = max(abs(TN - T));
   Z = ZN; W = WN; T = TN;
   ITER
   DIFFZ_DIFFT_DIFFW = [DIFFZ, DIFFT,DIFFW]
end
P = pressure_t(p,e,t,T,Z,Parmeter1);
save daten3c W Z T P Parmeter1
disp(' bild03 Aufrufen!')
