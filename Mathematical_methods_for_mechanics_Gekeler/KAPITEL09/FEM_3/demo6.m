function demo6
% Navier-Stokes problem
% Stream function vorticity method following H.Ninomiya/K.Onishi
% Example with exact solution, cf. Spotz, S. 3504
% X,Y   : Coordinates
% V     : Velocity at the center of triangle,
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% FF1   : File fuer erstes Gitter (entfaellt bei MATLAB TOOLBOX)
% FF2   : File fuer Geometrie in MATLAB Format 
% FF3   : File fuer Randbedingungen   

clear, clc, format short, format compact
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp01'; FF2 = 'bsp01g';
FF3 = 'bsp06h1';   % artificial boundary for W
%FF3 = 'bsp06h2';   % exact boundary for W (worse!)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MAXITER = 50;  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Parameters -------------------
DT = 0.02; NU = 0.01; % akzeptabel
%DT  = 0.01; NU = 0.001; % coeff. of viscosity [m*m/sec] % fails
VS     = 0;    %slip-boundary data from DATA.M
SEGNR  = [1,2,3,4]; % Segmentnrn. des Randes
REFINE = 5;    % Number of uniform refinements

Parmeter = [DT,NU,VS];
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' Cold start or restart ? (1/0) ');
end
if Start == 1
   [p,e,t] = feval(FF1); % erstes Gitter
   for J = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01_t(FF2,p,e,t);
        % p       = mesh10(p,e,t,4); % Jigglemesh
        % t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
   end
   RAND = e;
   %bild00(p,e,t,RAND)
   %pause
   disp(' Compute offset data ')
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL, 
   %pause
   % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
      disp(' Compute right side of exact example  ')
   flag = 1; 
   switch flag
   case 1
      % DATA = [z;w;u;v,deltaw;convecw];
      DATA = bsp06_data_a(p);
      Z0 = DATA(1,:)'; W0 = DATA(2,:)';
      RSIDE = -NU*DATA(5,:) + DATA(6,:);
   case 2
      % DATA = [z;w;u;v,rotf1; rotf2];
      DATA = bsp06_data_b(p);
      Z0 = DATA(1,:)'; W0 = DATA(2,:)';
      RSIDE = - NU*DATA(5,:) + DATA(6,:);
   end
   % -- boundary data ------------------------
   [RDZ,RCZ,RDW] = feval(FF3,e,Parmeter,W0);
   % -- initial values -------------------
   N = size(p,2); Z = zeros(N,1); W = Z;
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   save daten6a p e t RAND Parmeter DATA RSIDE MAXL
   save daten6b RDZ RCZ RDW NACHBAR NORMALEN 
end
load daten6a p e t RAND Parmeter DATA RSIDE MAXL
load daten6b RDZ RCZ RDW NACHBAR NORMALEN 
% ------------------------------
if Start ~= 1, load daten6c W Z; end; H = 1.2*MAXL; 
for ITER = 1:MAXITER
   ZN    = ellipt1(p,t,RDZ,RCZ,W);
   WBA   = wbound(p,e,RAND,t,RDW,W,Z,NACHBAR);
   WN    = vorticity1(p,t,WBA,W,ZN,Parmeter,RSIDE);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER, DIFFZ, DIFFW]
end
%V = velocity(p,e,t,Z);
save daten6c W Z
disp(' bild06 Aufrufen!')
%bild06
