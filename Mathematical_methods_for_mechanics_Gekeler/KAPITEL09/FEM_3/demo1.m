function demo1
% Navier-Stokes problem, Stream function vorticity method
% Example: lid driven cavity
% X,Y   : Coordinates
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% FF1   : File for first mesh (not used in MATLAB TOOLBOX)
% FF2   : File for geometry in MATLAB format 
% FF3   : File for boundary conditions  
% OPTION = 1/2 : Without or with MATLAB TOOLBOX 
% works for DT = 0.05; NU = 0.001; MAXITER = 500

clear, clc, format short, format compact
% Example:
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h';
% -- Parameter -------------------
MAXITER = 500;  % Step number 
DT     = 0.05;  % time step[sec]
NU     = 0.001; % coeff. of viscosity [m*m/sec]
% Choose DT = 0.025 for NU = 0.002 !!
VS     = 1;    % slip-boundary data
REFINE = 4;    % Number of uniform refinements
SEGNR  = [1,2,3,4]; % Segmentnrn. des Randes
Parmeter = [DT,NU,VS];
Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' New start or restart? (1/0) ');
end
if Start == 1, OPTION = 2;
   [p,e,t,RAND,INNERPKTE] = start4stream(FF1,FF2,OPTION,REFINE,SEGNR); 
   if OPTION == 2
      XLAENGE = 1; YLAENGE = 1;
      [p,e,t,RAND] = prepar2(p,e,t,FF2,XLAENGE,YLAENGE,SEGNR);
   end
   bild00(p,e,t,RAND)
   pause
   % -- Computation of offset data
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   % -- boundary data ------------------------
   [RDZ,RDW] = feval(FF3,e,Parmeter);
   save daten1a p e t RAND RDZ RDW
   save daten1c NACHBAR NORMALEN
end
load daten1a p e t RAND RDZ RDW
load daten1c NACHBAR NORMALEN
% ------------------------------
if Start == 1, W = zeros(size(p,2),1); Z = W;
else, load daten1b W Z; end
for ITER = 1:MAXITER
   ZN    = ellipt1(p,t,RDZ,[],W);
   WBA   = wbound(p,e,RAND,t,RDW,W,Z,NACHBAR);
   WN    = vorticity(p,t,WBA,W,ZN,Parmeter);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER, DIFFZ, DIFFW]
end
P = pressure(p,e,t,Z,Parmeter);
save daten1b P W Z Parmeter
disp(' Call bild01 !')
%bild01
