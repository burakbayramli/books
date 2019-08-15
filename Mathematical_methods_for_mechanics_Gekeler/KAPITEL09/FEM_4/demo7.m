function demo7
% Convection problem following H.Ninomiya/K.Onishi, p. 146
% Example: convection in a unit square with exact solution
% X,Y     : Coordinates
% V(1:2,:): Velocity at the center of triangle,
% Z/W     : Stream function/vorticity
% T       : Temperature
% SEGNR   : ordered segment numbers for boundary
% Eine Aenderung von VORTICITY_K.M ist nicht noetig, weil das
% exakte T mit der Gleichung fuer W konstruiert wurde!

clc, clear, format compact, format short e
%%%%%%%%% DATENEINTRAEGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% -- Example: Geometry file and file of boundary values ---
% Kuenstliche RB:
%REFINE=3; FF1='bsp03b'; FF2='bsp03gb'; FF3='bsp07h1'; DT=0.01; MAXITER = 20; 
REFINE=4; FF1='bsp03b'; FF2='bsp03gb'; FF3='bsp07h1'; DT=0.001; MAXITER = 200; 
% DT und MAXITER NICHT AENDERN!!

% Exacte RB: 
% REFINE = 3;
% FF1 = 'bsp03b'; FF2 = 'bsp03gb'; FF3 = 'bsp07h2'; DT = 0.1; MAXITER = 20;
% -- Parameter -------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
KAPPA  = 0;       % transfer coeff. [J/(m*s*K)] (not used)
g      = 9.81;    % gravitational acceleration [m/(s*s)]
T_AIR  = 0;      % outside temp. [K] or [deg]  (not used)
T0     = 0;      % init. temp. [K] or [deg]
SEGNR  = [1,2,3,4];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Im dim.-losen System ist NU = 1, LAMBDA = 1/PR, g*BETA = RA/PR
RA = 1.0E4 ; PR = 1;
NU = 1; LAMBDA = 1/PR; BETA = RA/(g*PR);
%%%% Geometrie aendern ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Parmeter = [DT,NU,LAMBDA,BETA,KAPPA,T_AIR];
Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' Initialization yes/no ? (1/0) ');
end
if Start == 1
     [p,e,t] = feval(FF1); % erstes Gitter
     for J = 1:REFINE
         disp(' Refinemesh ')
         [p,e,t] = mesh01_t(FF2,p,e,t);
      %   p       = mesh10(p,e,t,4); % Jigglemesh
      %   t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
      end
      RAND = e;
   % -- Computation of offset data
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   % -- data for inhomogeneous problem  -----
   % DATA = [z;w;t;delta_t;convec_t;delta_w;convec_w];
   DATA = bsp07_data(p);
   Z_EXACT = DATA(1,:)'; W_EXACT = DATA(2,:)'; T_EXACT = PR*DATA(3,:)'/RA;
   DELTA_T = PR*DATA(4,:)'/RA;  CONVEC_T = PR*DATA(5,:)'/RA;
   %DELTA_W = PR*DATA(6,:)'/RA;  CONVEC_W = PR*DATA(7,:)'/RA;
   [RDZ,RCZ,RDW,RDT,RCT] = feval(FF3,p,e,t,T_EXACT,W_EXACT);
   save daten7a p e t RAND NACHBAR NORMALEN 
   save daten7b RDZ RCZ RDW RDT RCT
   save daten7c T_EXACT Z_EXACT W_EXACT DELTA_T CONVEC_T% DELTA_W CONVEC_W
end
load daten7a p e t RAND NACHBAR NORMALEN 
load daten7b RDZ RCZ RDW RDT RCT
load daten7c T_EXACT Z_EXACT W_EXACT DELTA_T CONVEC_T% DELTA_W CONVEC_W
% ------------------------------
if Start == 1
   W = zeros(size(p,2),1); Z = W;
   T = T0*ones(size(p,2),1);
else, load daten10d V W Z T; end
for ITER = 1:MAXITER
   ZN  = ellipt1(p,t,RDZ,RCZ,W);
   WBA = wbound(p,e,RAND,t,RDW,W,ZN,NACHBAR);
   WN = vorticity_k(p,t,WBA,W,ZN,T,Parmeter);
   TN  = convection_r(p,t,ZN,RDT,RCT,T,Parmeter,DELTA_T,CONVEC_T);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   DIFFT = max(abs(TN - T));
   Z = ZN; W = WN; T = TN;
   ITER
   DIFFZ_DIFFT_DIFFW = [DIFFZ, DIFFT,DIFFW]
end
V = velocity(p,e,t,Z);
save daten7d V W Z T
disp(' bild07 Aufrufen!')
