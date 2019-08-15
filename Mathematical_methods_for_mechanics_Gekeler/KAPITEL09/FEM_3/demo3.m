function demo3
% Navier-Stokes problem, Stream function vorticity method
% Example: Flow past cylinder
% X,Y   : Coordinates
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% FF1   : File for first mesh (not used in MATLAB TOOLBOX)
% FF2   : File for Geometry in MATLAB format 
% FF3   : File for boundary conditions   
% OPTION = 1/2 : Without or with MATLAB TOOLBOX 

clear, clc, format short, format compact
% Example:
FF1 = 'bsp03'; FF2 = 'bsp03g'; FF3 = 'bsp03h';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Parameters -------------------
MAXITER = 50;
DT      = 0.1;  % time step[sec]
NU      = 0.001; % coeff. of viscosity [m*m/sec]
VS      = 0;    % slip-boundary data, not used
REFINE  = 4;    % Number of uniform refinements
SEGNR   = [1,7,8,9,3,4]; % Segm.-ns. Outer boundary
SEGNR1  = [5,6];         % Segm.-ns. Inner boundary 

Parmeter = [DT,NU,VS];
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' Cold start or Restart? (1/0) ');
end
%Start = 1;
if Start == 1, OPTION = 2;
   switch OPTION
   case 1, disp(' Mesh without TOOLBOX ')
     [p,e,t] = feval(FF1); % first mesh
     for J = 1:REFINE
         disp(' Refinemesh ')
         [p,e,t] = mesh01_t(FF2,p,e,t);
         p       = mesh10(p,e,t,4); % Jigglemesh
      %   t       = mesh03(p,t,0);   % Replace long edges
      end
      RAND = [];
      for I = 1:length(SEGNR)
         J  = find(e(5,:) == SEGNR(I)); EE = e(:,J);
         %[U,K] = sort(EE(3,:)); EE = EE(:,K);
         RAND = [RAND,EE];
      end
      if ~isempty(SEGNR1)
         for I = 1:length(SEGNR1)
            J  = find(e(5,:) == SEGNR1(I)); EE = e(:,J);
            %[U,K] = sort(EE(3,:)); EE = EE(:,K);
            RAND = [RAND,EE];
         end
      end
   case 2, disp(' Mesh with TOOLBOX ')
      [p,e,t,RAND,INNERPKTE] = prepar(FF2,REFINE,[SEGNR,SEGNR1]);
      % -- Computation of offset data
   end
   bild00(p,e,t,RAND)
   pause
   % -- Computation of offset data
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Normalenlaenge
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   if ~isempty(SEGNR1) 
      [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR1,t);
      LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
      [NACHBAR1,NORMALEN1,ecode] = mesh43(p,e,SEGNR1,OFFSET,LAENGE);
      NACHBAR = [NACHBAR,NACHBAR1];
      NORMALEN = [NORMALEN,NORMALEN1];
   end
   % -- boundary data ------------------------
   [RDZ,RDW] = feval(FF3,e,Parmeter);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   save daten3a p e t RAND RDZ RDW
   save daten3b NACHBAR NORMALEN
end
load daten3a p e t RAND RDZ RDW
load daten3b NACHBAR NORMALEN
% ------------------------------
if Start == 1, W = zeros(size(p,2),1); Z = W;
else, load daten3c W Z; end
for ITER = 1:MAXITER
   ZN    = ellipt1(p,t,RDZ,[],W);
   WBA   = wbound(p,e,RAND,t,RDW,W,Z,NACHBAR);
   WN    = vorticity(p,t,WBA,W,ZN,Parmeter);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER, DIFFZ, DIFFW]
end
save daten3c W Z
disp(' Call bild03 !')
%bild03
