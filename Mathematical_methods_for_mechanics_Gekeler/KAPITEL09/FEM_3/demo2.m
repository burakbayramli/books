function demo2
% Navier-Stokes problem, Stream-function vorticity method 
% Example: flow past half cylinder
% X,Y   : Coordinates
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% FF1   : File fuer erstes Gitter (entfaellt bei MATLAB TOOLBOX)
% FF2   : File fuer Geometrie in MATLAB Format 
% FF3   : File fuer Randbedingungen   
% OPTION = 1/2 : Without or with MATLAB TOOLBOX 

clear, clc, format short, format compact
Example = 1;
switch Example
case 1, % Example (a):
   FF1 = 'bsp02'; FF2 = 'bsp02g'; FF3 = 'bsp02h';
   % -- Parameters -------------------
   MAXITER = 50;  % Step number, 
   LOOP = 4; 
   DT     = 0.1;  % time step[sec] 
   NU     = 0.01; % coeff. of viscosity [m*m/sec] 
   REFINE = 4;    % Number of uniform mesh refinements
case 2, % Example (b):
   FF1 = 'bsp02'; FF2 = 'bsp02gn'; FF3 = 'bsp02h';
   % -- Parameters -------------------
   MAXITER = 50;  % Step number, 50*8 = 400 total step number
   LOOP = 8; % 8*50 = 400 total step number
   DT     = 0.01;  % time step[sec] 
   NU     = 0.001; % coeff. of viscosity [m*m/sec]
   REFINE = 4;    % Number of uniform mesh refinements
end
% -- Parameters -------------------
VS     = 0;    % slip-boundary data, not used
SEGNR  = [1,2,3,7,8,9,5,6]; % Segmentns. of boundary
Parmeter = [DT,NU,VS];
disp(' Make first mesh with OPTION = 1, then cold start with OPTION = 2 ')
disp(' then several restarts wit OPTION = 3 ') 
Start = 100; 
while ~ismember(Start,[1,2,3])
   Start = input(' Mesh, cold start, or restart? (1/2/3) ');
end
switch Start
case 1, disp(' Make mesh '), OPTION = 2; 
   [p,e,t,RAND] = start4stream(FF1,FF2,OPTION,REFINE,SEGNR); 
   bild00(p,e,t,RAND)
   %pause
    if OPTION == 2 & Example == 2 
      [p,e,t,RAND] = prepar4(p,e,t,FF2,SEGNR);
    end
    Nodes = size(p,2)
   %pause
   % -- Computation of offset data
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL, % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   % -- boundary data ------------------------
   [RDZ,RDW] = feval(FF3,e,Parmeter);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   save daten2a p e t RAND RDZ RDW 
   save daten2b NACHBAR NORMALEN
   return
case 2, disp(' Cold start ')   
   load daten2a p e t RAND RDZ RDW
   load daten2b NACHBAR NORMALEN
   W = zeros(size(p,2),1); Z = W;
   for ITER = 1:MAXITER
      ZN    = ellipt1(p,t,RDZ,[],W);
      WBA   = wbound(p,e,RAND,t,RDW,W,Z,NACHBAR);
      WN    = vorticity(p,t,WBA,W,ZN,Parmeter);
      DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
      Z = ZN; W = WN;
      ITER_DIFFZ_DIFFW = [ITER, DIFFZ, DIFFW]
   end
   save daten2c W Z
   bild02
   FILM(1) = getframe;
case 3, disp(' LOOP ' )
   load datenfilm2 FILM
   load daten2a p e t RAND RDZ RDW
   load daten2b NACHBAR NORMALEN
   load daten2c W Z 
   for K = 2:LOOP 
      load daten2c W Z 
      for ITER = 1:MAXITER
         ZN    = ellipt1(p,t,RDZ,[],W);
         WBA   = wbound(p,e,RAND,t,RDW,W,Z,NACHBAR);
         WN    = vorticity(p,t,WBA,W,ZN,Parmeter);
         DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
         Z = ZN; W = WN;
         K_ITER_DIFFZ_DIFFW = [K,ITER, DIFFZ, DIFFW]
      end
      save daten2c W Z
      bild02
      FILM(K) = getframe;
   end
end
save datenfilm2 FILM
