function demo4
% Masterfile for shallow water problem
% Long wave on a beach
% cf. J.Petera/V.Nassehi: Int. J. Numer. Meth. Eng. 39, 4159-4182
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WITHOUT PDE TOOLBOX %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% p = (p1;p2;p3): (X,Y)-Coordinates, water depth
% V = (V1;V2;V3): V1 = U, V2 = V Velocity;
% V3 = Z        : Tidal elevation
% FF1           : Geometrie-File
% FF2           : File der Randbedingungen 
clc, clear, format compact
disp(' Beispiel 4, Long Wave on beach')
%%%% Parametereingabe  %%%%%%%%%%%%%%%%%%%%%%%%%%%
FF1 = 'bsp04'; FF2 = 'bsp04h'; 
HOURS  =  1/144;    % 25 Sek. simulation time total [hour]
DT     =  0.05; % time interval for computation [sec] bei flow_3.m
%DT    =  1; % time interval for computation [sec] bei flow_1a.m
PERIOD = 300;    % Periode am offenen linken Ende [sec]
A      = 0.3;     % Amplitude of inlet (not used)
g      = 1;      % gravitational acceleration (Petera)
%%% Ende der Parametereingabe %%%%%%%%%%%%%%%%%%%%
Parmeter = [A,PERIOD,g];
% Iteration ueber NN Perioden
NN      = HOURS*3600/PERIOD; 
MAXITER = 3600*HOURS/DT;
%% Netzerzeugung %%%%%%%%%%%%%%%%
[p,e,t] = feval(FF1); N = size(p,2);
RDSEA  = 4;         % Segnrn. sea
RDLAND = [1,3];     % Segnrn. land
RDOUT  = 2;         % Outflow
RD     = [1,2,3,4]; % Segnrn. for outer boundary (closed)
save daten4a p e t
%%% Warm Start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
V = zeros(3,N); VN = V;
JJ = [2,2+3*[1:40]];
MONITOR_Z = []; MONITOR_U = [];
% -- initial condition ---------------------
A0 = 0.1; alfa = 1/30; XX = linspace(0,40,41);
AUX = 0.5*sqrt(3*A0)*(XX - 1/alfa);
Z0 = A0./(cosh(AUX).*cosh(AUX));
U0 = -(1 + 0.5*A0)*Z0./(alfa*XX + Z0); U0(1) = 0;
H0 = linspace(0,4/3,41);
NN = [1,2,3];
V(3,NN) = Z0(1); V(1,NN) = U0(1); p(3,NN) = H0(1);
for I = 1:40
    NN = NN + 3;
    V(3,NN) = Z0(I+1); V(1,NN) = U0(I+1);
    p(3,NN) = H0(I+1);
end
VN = V;
% Simulation start ------------------------------------
T = 0;
clf, hold on, axis([0 4 -0.2 0.2]), axis manual
for ITER = 1:MAXITER
   VN = flow_3(p,t,V,VN,DT/2,Parmeter);   % full mass matrix
%   VN = flow_1a(p,t,V,VN,DT/2,Parmeter); % lumped mass matrix
%   VN = flow_1(p,t,V,VN,DT/2,Parmeter);  % fails
   T = T + DT/2; 
   VN = bsp04h(p,e,VN,T,Parmeter);
   SHALLOW = find(p(3,:) + VN(3,:) <= 0); % shallow water
   VN(1:2,SHALLOW) = 0;
   % ------------------------------
   VN = flow_3(p,t,V,VN,DT,Parmeter);   % full mass matrix
%   VN = flow_1a(p,t,V,VN,DT,Parmeter); % lumped mass matrix
%   VN = flow_1(p,t,V,VN,DT,Parmeter);  % fails
    T = T + DT/2;
   VN = bsp04h(p,e,VN,T,Parmeter);
   SHALLOW = find(p(3,:) + VN(3,:) <= 0); % shallow water
   VN(1:2,SHALLOW)    = 0;
   V = VN;
   if abs(T - round(T)) < 1E-10
      MAXITER_ITER_SEK = [MAXITER,ITER,T]
      XX = p(1,JJ);
      plot(XX/10,V(3,JJ),'b'), hold on
      plot(XX/10,V(1,JJ),'k'), hold on
      grid on
      pause(0.05)
      %set(gca,'nextplot','replacechildren');
      MONITOR_U = [MONITOR_U;V(1,JJ)];
      MONITOR_Z = [MONITOR_Z;V(3,JJ)];
   end
end
save daten4c MONITOR_Z MONITOR_U
disp(' bild04 Aufrufen! ')
