function dem08
% Example: Benard Cell
% unsteady scaled natural convection
% X,Y     : Coordinates
% V(1:2,:): Velocity at the center of triangle,
% Z/W/T   : Stream function/vorticity/Temperature
% SEGNR   : ordered segment numbers for boundary

clc, clear, format short e
FF1 = 'bsp08g'; FF2 = 'bsp08h';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MAXITER = 100; DT = 0.1; % time step [sec]
disp(' Insgesamt mind. 10-mal Restart ! ')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Parameters  for Water -------------------
NU     = 1.8E-6;  % coeff. of viscosity [m*m/s] 
LAMBDA = 1.35E-7; % thermal diffusivity [m*m/s] 
BETA   = 4.6E-4;  % volume expansion coeff. [1/K];
g      = 9.81;    % gravitational acceleration
KAPPA  = 0;       % transfer coeff. [J/(m*s*K)] (not used)
T_AIR   = 0;      % outside temp. [K] or [deg] (not used)
T0     = 0;       % init. temp. [K] or [deg]
REFINE = 3;       % Number of uniform refinements
SEGNR  = [1,2,3,4];
DELTA_T = 1; DELTA_L = 0.01; 
PR = NU/LAMBDA;
RA = g*BETA*DELTA_T*DELTA_L^3/(NU*LAMBDA)
%% Im dim.-losen System ist
% t := t*NU_ALT/DELTA_L^2, NU = 1, LAMBDA = 1/PR, g*BETA = RA/PR = GR
% also
DT = DT*NU/DELTA_L^2
NU = 1; LAMBDA = 1/PR, BETA = RA/(g*PR); T0 = 0;
TEMP_U = 1; TEMP_O = 0;
TEMP_U = 334; TEMP_O = 333;  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Parmeter = [DT,NU,LAMBDA,BETA,KAPPA,T_AIR,g,TEMP_U,TEMP_O];
SEGNR_A  = [37,34,31,28,25,22,19,16,13,10,7,3];
SEGNR_B = 4;
SEGNR_C = [1,5,8,11,14,17,20,23,26,29,32,35];
SEGNR_D = 36;
SEGNR = [SEGNR_A,SEGNR_B,SEGNR_C,SEGNR_D];
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' Initialization yes/no ? (1/0) ');
end
%Start = 0;
if Start == 1
   [p,e,t,RAND,IP] = prepar(FF1,REFINE,SEGNR);
      % -- Computation of offset data
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   % -- boundary data ------------------------
   [RDZ,RCZ,RDW,RDT,RCT] = feval(FF2,p,e,t,SEGNR,Parmeter);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   save daten8a p e t RAND Parmeter
   save daten8b RDZ RCZ RDW RDT RCT NACHBAR NORMALEN
end
load daten8a p e t RAND Parmeter
load daten8b RDZ RCZ RDW RDT RCT NACHBAR NORMALEN
% ------------------------------
for I = 1:5
if Start == 1
   W  = zeros(size(p,2),1); Z = W;
   T = T0*ones(size(p,2),1);
else, load daten8c V W Z T; end
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
V = velocity(p,e,t,Z);
save daten8c V W Z T
disp(' bild08 Aufrufen!')
bild08
pause
end
