function demo4
% Navier-Stokes problem, Stream function vorticity method
% Example: back facing step, unscaled
% X,Y   : Coordinates
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% FF1   : File for first mesh (not used in MATLAB TOOLBOX)
% FF2   : File for geometry in MATLAB Format 
% FF3   : File for boundary conditions   
% OPTION = 1/2 : Without or with MATLAB TOOLBOX 

clear, clc, format short, format compact
% Example:
FF1 = 'bsp04'; FF2 = 'bsp04g'; FF3 = 'bsp04h';
% -- Parameter -------------------
MAXITER = 50;      % Step number
DT     = 7.5E-5;   % time step[sec]
NU     = 1.338E-5; % coeff. of viscosity [m*m/sec] 
VS     = 0;        % slip-boundary data (not used)
REFINE = 3;    % Number of uniform refinements

SEGNR  = [6 7 8 9 14 19 24 29 34 39 44 45 42 43 38 33 28 23 18 13 3 5];
Parmeter = [DT,NU,VS];
Start = 100; KK = [0,1];
while ~ismember(Start,[0,1])
   Start = input(' Cold start or restart? (1/0) ');
end
if Start == 1, OPTION = 2;
   [p,e,t,RAND] = start4stream(FF1,FF2,OPTION,REFINE,SEGNR); 
   % partial refinement:
   if OPTION == 2
     [p,e,t,RAND] = prepar3(p,e,t,FF2,SEGNR);
   end
   bild00(p,e,t)
   pause
   disp(' Computation of offset data ')
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   % -- boundary data ------------------------
   [RDZ,RDW] = feval(FF3,e,Parmeter);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   save daten4a p e t RAND Parmeter
   save daten4b RDZ RDW NACHBAR NORMALEN
end
load daten4a p e t RAND Parmeter
load daten4b RDZ RDW NACHBAR NORMALEN
% ------------------------------
if Start == 1, W = zeros(size(p,2),1); Z = W;
else, load daten4c W Z; end

for ITER = 1:MAXITER
   ZN    = ellipt1(p,t,RDZ,[],W);
   WBA   = wbound(p,e,RAND,t,RDW,W,Z,NACHBAR);
   WN    = vorticity(p,t,WBA,W,ZN,Parmeter);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER,DIFFZ, DIFFW]
end
save daten4c W Z
disp(' bild04 Aufrufen!')
%bild04
