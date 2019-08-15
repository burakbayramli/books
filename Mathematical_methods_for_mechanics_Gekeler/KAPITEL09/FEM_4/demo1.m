function demo1
% Example: thermal flow in a cup
% unsteady unscaled convection 
% X,Y     : Coordinates
% V(1:2,:): Velocity at the center of triangle,
% Z/W     : Stream function/vorticity
% T       : Temperature
% SEGNR   : ordered segment numbers for boundary

clc, clear, format short, format compact
%%%% DATENEINGABE %%%%%%%%%%%%%%%%%%%%%%%%%
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp01'; FF2 = 'bsp01h';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MAXITER = 150;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Parameter -------------------
DT     = 0.2;     % time step [s]
NU     = 0.05;    % coeff. of viscosity [m*m/s], Ra = 1.0E3
NU     = 0.016;   % coeff. of viscosity [m*m/s], Ra = 1.0E4
NU     = 0.005;   % coeff. of viscosity [m*m/s], Ra = 1.0E5
LAMBDA = NU;      % thermal conductivity [m*m/s]
BETA   = 0.21E-3; % volume expansion coeff. [1/K]
KAPPA  = 0.04;    % heat transfer coeff [J/(m*s*K)]
T_AIR  = 15;      % outside temp. [K] or [deg]
T0     = 15;      % init. temp. [K] or [deg]
%REFINE = 4;       % Number of uniform refinements
SEGNR  = [1,2,3,4];
%% ENDE DER DATENEINGABE %%%%%%%%%%%%%%%%% 
Parmeter = [DT,NU,LAMBDA,BETA,KAPPA,T_AIR];
[p,e,t] = feval(FF1); RAND = e;
% -- Computation of offset data
[RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
[NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
% -- boundary data ------------------------
[RDZ,RCZ,RDW,RDT,RCT] = feval(FF2,p,e,t,Parmeter);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save daten1a p e t RAND Parmeter
save daten1b RDZ RDW RDT RCT NACHBAR NORMALEN
% ------------------------------
W  = zeros(size(p,2),1); Z = W;
T = T0*ones(size(p,2),1);
for ITER = 1:MAXITER
   ZN  = ellipt1(p,t,RDZ,RCZ,W);
   WBA = wbound(p,e,RAND,t,RDW,W,ZN,NACHBAR);      % Randbedg. fuer W
   WN  = vorticity_k(p,t,WBA,W,ZN,T,Parmeter);  % Gl. fuer W
   TN  = convection(p,t,ZN,RDT,RCT,T,Parmeter); % Gl. fuer T
   DIFFZ = max(abs(ZN - Z)); DIFFT = max(abs(TN - T));
   DIFFW = max(abs(WN - W));
   Z = ZN; W = WN; T = TN;
   ITER_DIFFZ_DIFFT_DIFFW = [ITER, DIFFZ, DIFFT, DIFFW]
end
V = velocity(p,e,t,Z);
save daten1c V W Z T
disp(' bild01 Aufrufen!')
%bild01
