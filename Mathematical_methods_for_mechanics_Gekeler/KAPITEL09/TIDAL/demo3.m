function demo3
% Masterfile for shallow water problem
% Open Channel cf. J.Petera/V.Nassehi without PDE TOOLBOX
% p = (p1;p2;p3) : (X,Y)-Coordinates, water depth
% V = (V1;V2;V3) : V1 = U, V2 = V Velocity;
% V3 = Z         : Tidal elevation
% FF1        : Geometrie-File
% FF2        : File der Randbedingungen 
clc, clear, format compact
disp(' Beispiel 3: Long Channel ')
%%% Parametereingabe %%%%%%%%%%%%%%%%%%%%%
FF1 = 'bsp03'; FF2 = 'bsp03h';
HOURS = 1/6;   % 6000 Sek.
DT       = 0.5; % time interval for computation [sec]
PERIOD   = 300;  % cycle for inlet [sec]
A        = 0.3;  % Amplitude of inlet
g        = 10;   % gravitational acceleration
%%% Ende der Parametereingabe %%%%%%%%%%%%%
Parmeter = [A,PERIOD,g];
NN = HOURS*3600/PERIOD; % Iteration ueber NN Perioden
MAXITER = HOURS*3600/DT; % MAXITER*DT simulation time total [sec]
%%% Netzerzeugung %%%%%%%%%%%%%%%%%%%%%%%%%%%
[p,e,t,MESSPKT] = feval(FF1); N = size(p,2);
save daten3a p e t MESSPKT
RDLAND = [1,3]; % Segnrn. land
RDOUT  = 2;     % Outflow
RDSEA  = 4;     % Segnrn. sea
RD     = [1,2,3,4]; % Segnrn. for outer boundary (closed)
%%%% Cold Start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
V = zeros(3,N); VN = V;
JJ = [2,2+3*[1:30]]; MONITOR_Z = [];
T = 0; TIME = [];
clf, hold on
axis([0 35 -0.5 0.5]), axis manual
% Simulation start ------------------------------------
for ITER = 1:MAXITER
   VN = flow_3(p,t,V,VN,DT/2,Parmeter);
   T = T + DT/2;
   [RDU,RDV,RDZ] = feval(FF2,p,e,VN,T,Parmeter);
   VN(1,RDU(1,:)) = RDU(2,:);   VN(2,RDV(1,:)) = RDV(2,:);
   VN(3,RDZ(1,:)) = RDZ(2,:);

   SHALLOW = find(p(3,:) + VN(3,:) <= 0); % shallow water
   VN(1:2,SHALLOW)    = 0;
   % ------------------------------
   VN = flow_3(p,t,V,VN,DT,Parmeter);
   T = T + DT/2;
   [RDU,RDV,RDZ] = feval(FF2,p,e,VN,T,Parmeter);
   VN(1,RDU(1,:)) = RDU(2,:);   VN(2,RDV(1,:)) = RDV(2,:);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   SHALLOW = find(p(3,:) + VN(3,:) <= 0); % shallow water
   VN(1:2,SHALLOW)    = 0;
   V = VN;
   if T - floor(T) == 0, MAXIT_ITER_SEC = [MAXITER,ITER,T], end
   % ----------------------------
   grid on
   plot(V(3,JJ),'k'), hold on
   plot(V(1,JJ),'r'), hold on
   set(gca,'nextplot','replacechildren')
   pause(0.01)
   if T >= 300
      TIME = [TIME,T];
      MONITOR_Z = [MONITOR_Z,VN(3,MESSPKT)];
   end
end
save daten3b TIME MONITOR_Z V Parmeter
disp(' bild03 Aufrufen ! ') 
%bild03
