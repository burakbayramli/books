function demo1b
% Masterfile for shallow water problem
% Island in a bay; following H.Ninomiya/K.Onishi
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% flow at coast by own computation %%%
%%% Different Results                %%%
%%% WITHOUT MATLAB PDE TOOLBOX       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% p = (p1;p2;p3) : (X,Y)-Coordinates, water depth
% V = (V1;V2;V3) : V1 = U, V2 = V Velocity;
% V3 = Z         : Tidal elevation
% FF1        : Geometrie-File
% FF2        : File der Randbedingungen 
clc, clear, format compact
disp(' Beispiel 1: Island in a Bay ')
%%% Parametereingabe %%%%%%%%%%%%%%%%%%%%%
FF1 = 'bsp01'; FF2 = 'bsp01h';
DT      = 60;            % time interval for computation [sec]
HOURS   = 6;            % simulation time total [hour]
PERIOD = 12*3600;       % cycle [sec] of inlet
A      = 1;              % Amplitude of inlet
g      = 10;             % gravitational acceleration
%%% Ende der Parametereingabe %%%%%%%%%%%%%%%%%
Parmeter = [A,PERIOD,g];
[p,e,t]  = feval(FF1);
save daten1a p e t
RDSEA    = 1;     % Segnrn. sea
RDLAND   = 2;     % Segnrn. land
RDISLAND = 3;     % Segnrn. island
MAXITER  = HOURS*3600/DT; % TLIMIT*DT simulation time total [sec]
% ----------------------------------------------------
N = size(p,2); V = zeros(3,N); VN = V;
TANGENTS1 = mesh44(p,e,t,RDLAND);
TANGENTS2 = mesh44(p,e,t,RDISLAND);
% Simulation start ------------------------------------
T = 0; Stunden = 0;
for ITER = 1:MAXITER
  % VN = flow_1(p,t,V,VN,DT/2,Parmeter);
   VN = flow_2(p,t,V,VN,DT/2,Parmeter);
   T   = T + DT/2;
   RDZ = feval(FF2,e,T,Parmeter);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   VN(1:2,:) = vnomal_n(e,VN(1:2,:),TANGENTS1,RDLAND);
   VN(1:2,:) = vnomal_n(e,VN(1:2,:),TANGENTS2,RDISLAND);
   SHALLOW   = find(p(3,:) + VN(3,:) <= 0); % shallow water
   VN(1:2,SHALLOW) = 0;
   % ------------------------------
  % VN = flow_1(p,t,V,VN,DT,Parmeter);
   VN = flow_2(p,t,V,VN,DT,Parmeter);
   T   = T + DT/2;
   RDZ = feval(FF2,e,T,Parmeter);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   VN(1:2,:) = vnomal_n(e,VN(1:2,:),TANGENTS1,RDLAND);
   VN(1:2,:) = vnomal_n(e,VN(1:2,:),TANGENTS2,RDISLAND);
   SHALLOW   = find(p(3,:) + VN(3,:) <= 0); % shallow water
   VN(1:2,SHALLOW) = 0;
   V = VN;
   IT_STD = ITER - floor(ITER/(3600/DT))*(3600/DT);
   if IT_STD == 0
      Stunden = Stunden + 1
      switch Stunden
         case  1, save daten1b_1  V SHALLOW
         case  2, save daten1b_2  V SHALLOW
         case  3, save daten1b_3  V SHALLOW
         case  4, save daten1b_4  V SHALLOW
         case  5, save daten1b_5  V SHALLOW
         case  6, save daten1b_6  V SHALLOW
         case  7, save daten1b_7  V SHALLOW
         case  8, save daten1b_8  V SHALLOW
         case  9, save daten1b_9  V SHALLOW
         case 10, save daten1b_10 V SHALLOW
         case 11, save daten1b_11 V SHALLOW
         case 12, save daten1b_12 V SHALLOW
      end
   end
end
save daten1b V SHALLOW
disp(' bild01a oder bild01b Aufrufen! ')
