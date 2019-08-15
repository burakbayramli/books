function demo9a
% vgl. DEMO5.M in FEM_3
% Navier-Stokes problem
% Stream function vorticity method following H.Ninomiya/K.Onishi
% Example: Navier Stokes preparation for transport problem
% X,Y   : Coordinates
% V     : Velocity at the center of triangle,
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary

clear, clc, format short, format compact
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp09'; FF2 = 'bsp09ha';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MAXITER = 300; DT = 0.1; % 30 sec
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Parameters -------------------
NU     = 1.0E-3; % coeff. of viscosity [m*m/sec]
VS     = 0;       % slip-boundary data
REFINE = 0;      % Number of uniform refinements
SEGNR  = [1 2 3 4 5 6]; % Segmentnrn. des Randes
Parmeter = [DT,NU,VS];
Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' Initialization yes/no ? (1/0) ');
end
if Start == 1
   [p,e,t] = feval(FF1); RAND = e;   % erstes Gitter
   for J = 1:REFINE
      [p,e,t] = mesh01_t([],p,e,t);
   end   
   RAND = e;
   disp(' Computation of offset data ')
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   % -- boundary data ------------------------
   [RDZ,RDW] = feval(FF2,e,Parmeter);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   save daten9a p e t RAND Parmeter
   save daten9b RDZ RDW NACHBAR NORMALEN
end
load daten9a p e t RAND Parmeter
load daten9b RDZ RDW NACHBAR NORMALEN
% ------------------------------
RDCAUCHY = [];
if Start == 1, W = zeros(size(p,2),1); Z = W;
else, load daten9c V W Z; end
for ITER = 1:MAXITER
   ZN    = ellipt1(p,t,RDZ,RDCAUCHY,W);
   WBA  = wbound(p,e,RAND,t,RDW,W,Z,NACHBAR);
   WN    = vorticity(p,t,WBA,W,ZN,Parmeter);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER, DIFFZ, DIFFW]
end
V = velocity(p,e,t,Z);
save daten9c V W Z
disp(' bild09a Aufrufen!')
%bild09a
