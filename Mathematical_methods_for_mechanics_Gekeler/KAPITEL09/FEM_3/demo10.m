function demo10
% Navier-Stokes problem
% Stream function vorticity method following H.Ninomiya/K.Onishi
% simple iteration, solution with ODE23.M
% Example with exact solution, cf. Spotz, S. 3504
% X,Y   : Coordinates
% V     : Velocity at the center of triangle,
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary

clc, clear, format short, format compact, errorcode = 0;
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp01'; FF2 = 'bsp01g';
% -- Choose OPTION !! ----------------------------
FF3 = 'bsp06h1'; % W artificial on boundary
FF3 = 'bsp06h2'; % W exact on boundary
% -- Parameters -------------------
DT = 1; MAXITER = 10;
% gleiches Ergebnis bei DT = 0.5, MAXITER = 20;
NU     = 0.01; % coeff. of viscosity [m*m/sec]
VS     = 0;    %slip-boundary data (not used)
SEGNR  = [1,2,3,4]; % Segmentns. of boundary
REFINE = 4;         % Number of uniform refinements
Start = 100; 
Parmeter = NU;
while ~ismember(Start,[0,1])
   Start = input(' Cold start or restart ? (1/0) ');
end
if Start == 1
   disp(' Computation of geometry data ')
   [p,e,t] = feval(FF1); % first mesh
   for J = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01_t(FF2,p,e,t);
%     p       = mesh10(p,e,t,4); % Jigglemesh
%     t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
   end
 %  bild00(p,e,t)
 %  pause
 %  RAND = e;
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   disp(' Compute offset data ')
   [RDKN,RDEL,OFFSET,MAXL] = mesh40(p,e,SEGNR,t);
   LAENGE = 1.2*MAXL; % Length of normals in bound for SCHNITT.M
   [NACHBAR,NORMALEN,ecode] = mesh43(p,e,SEGNR,OFFSET,LAENGE);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   disp(' Compute right side of exact example  ')
   flag = 1; % beide Optionen gleich bei refine = 3;
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
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   disp(' Compute boundary values ')
   [RDZ,RCZ,RDW] = feval(FF3,e,Parmeter,W0);
   [KK,MM,Q] = matrizen1(p,e,t,RCZ);
   RSIDE = MM*RSIDE';
   save daten10a p e t KK MM RDZ RDW NACHBAR
   save daten10b RSIDE NU DATA
   W = zeros(size(p,2),1);
   T_END = DT; tspan = [0,T_END];
else
   load daten10b RSIDE NU DATA
   load daten10a p e t KK MM RDZ RDW NACHBAR
   load daten10c W Z tspan
end
disp(' Solve problem ')
options = odeset('RelTol',1E-4); % fuer ode23.m
for I = 1:MAXITER
   [T,WN] = ode23(@rside10,tspan,W,options,NU,RSIDE);
   tspan = tspan + DT;
   LM    = size(WN,1); WN = WN(LM,:)';
   Z     = ellipt1(p,t,RDZ,[],WN);
   WBA   = wbound(p,e,e,t,RDW,WN,Z,NACHBAR);
   WN(WBA(1,:)) = WBA(2,:)';
   DIFFW = max(abs(WN - W));
   W     = WN;
   ITER_DIFFW = [I,DIFFW]
end
Z = ellipt1(p,t,RDZ,[],W);
save daten10c W Z tspan
disp(' bild10 Aufrufen!')
%bild10
