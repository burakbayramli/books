function demo5
% Masterfile for shallow water problem
% Channel problems, constant water depth
% p = (p1;p2;p3) : (X,Y)-Coordinates, water depth
% V = (V1;V2;V3) : V1 = U, V2 = V Velocity;
% V3 = Z         : Tidal elevation
% XSCALE  scale factor in X-axis,
% YSCALE  scale factor in Y-axis
% L       Channel length [m]
% H       Water depth [m]
% PERIOD  Period at open (left) end [sec]
% HOURS   Simulation time total [hour]
% A       Amplitude [m]
% DT      Time step [sec]
% NN      Number of periods, Condition NN*Period = 3600 
% ---------------------------------
clc, clear, format compact, format short
%Example = 100; while ~ismember(Example,[1,2,3,4])
%Example = input(' Which example? (1/2/3/4)'); end

Example = 1;

Start = 100; while ~ismember(Start,[0,1])
Start = input(' Cold start/restart? (1/0)'); end

switch Example
case 1
   %FF1 = 'bsp03'; FF2 = 'bsp02h'; XSCALE  = 4/3; YSCALE  = 8; 
   FF1 = 'bsp02'; FF2 = 'bsp02h'; 
   XSCALE  = 100; YSCALE  = 100; L = 4000; H = 20;
   PERIOD = 3600; A = 0.5; DT = 10;
   HOURS = 1; NN = HOURS*3600/PERIOD;
case 2    
   %FF1 = 'bsp03'; FF2 = 'bsp02h'; XSCALE  = 4/3; YSCALE  = 8; 
   FF1 = 'bsp02'; FF2 = 'bsp02h1'; XSCALE  = 25; YSCALE  = 100;
   L = 1000; H = 20;
   PERIOD = 360; A = 1; DT = 5;
   HOURS = 1; NN = HOURS*3600/PERIOD;
end   
NU = 2.3*1E-2;    % Eddy-Viskositaet/Mass density 
g   = 10; % 9.81 Erdbeschleunigung [m/sec^2]
TAU = 2.3E-4;    % TAU = 1/C^2 [s^2/m]  (not used)

[p,e,t] = feval(FF1);  % erstes Gitter
%[p,e,t] = mesh01_t([],p,e,t); %FAILS !!
p(1,:)  = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:);
N = size(p,2); p = [p;H*ones(1,N)]; 
%bild00(p,e,t), pause

switch Start
case 1
   V = zeros(3,N); VN = V; T = 0; % Cold start
   MONITOR_U = zeros(1,N); MONITOR_V = zeros(1,N); MONITOR_Z = zeros(1,N);
   Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN,NU];
   Parmeter_flow = [A,PERIOD,g,NU];
   save daten5a p e t Parmeter FF2 Example HOURS
case 0
   load daten5a p e t Parmeter FF2 Example HOURS
   switch Example
      case 1, load daten5b MONITOR_U MONITOR_V MONITOR_Z T
      case 2, load daten5c MONITOR_U MONITOR_V MONITOR_Z T
      case 3, load daten5d MONITOR_U MONITOR_V MONITOR_Z T
      case 4, load daten5e MONITOR_U MONITOR_V MONITOR_Z T
   end
   AUXU = MONITOR_U(end,:); AUXV = MONITOR_V(end,:);
   AUXZ = MONITOR_Z(end,:);
   V = [AUXU;AUXV;AUXZ]; VN = V;
   HOURS = HOURS + Parmeter(11);
   Parmeter_flow = Parmeter([2,5,6,12]);
   save daten5a p e t Parmeter FF2 Example HOURS
end
MAXITER = NN*PERIOD/DT;

% Simulation start ------------------------------------
for ITER = 1:MAXITER
   T = T + DT/2; DTT = DT/2;
   %VN = flow_3(p,t,V,VN,DTT,Parmeter_flow);  % FAILS!
   %VN = flow_1a(p,t,V,VN,DTT,Parmeter_flow); % Weak
   %VN = flow_2(p,t,V,VN,DTT,Parmeter_flow);  % Best
   VN = flow_4(p,t,V,VN,DTT,Parmeter_flow);  % Best

   [RDU,RDV,RDZ] = feval(FF2,e,T,Parmeter);
   VN(1,RDU(1,:)) = RDU(2,:); VN(2,RDV(1,:)) = RDV(2,:);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   %SHALLOW = find(H + VN(3,:) <= 0); % shallow water
   %VN(1:2,SHALLOW)    = 0;
   % ------------------------------
   T = T + DT/2; DTT = DT;
   %VN = flow_3(p,t,V,VN,DTT,Parmeter_flow);  % FAILS!
 %  VN = flow_1a(p,t,V,VN,DTT,Parmeter_flow); % Weak  
  % VN = flow_2(p,t,V,VN,DTT,Parmeter_flow);  % Best
   VN = flow_4(p,t,V,VN,DTT,Parmeter_flow);   % Best

   [RDU,RDV,RDZ] = feval(FF2,e,T,Parmeter);
   VN(1,RDU(1,:)) = RDU(2,:); VN(2,RDV(1,:)) = RDV(2,:);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   %SHALLOW = find(H + VN(3,:) <= 0); % shallow water
   %VN(1:2,SHALLOW)    = 0;
   V = VN;
   MONITOR_U = [MONITOR_U;V(1,:)]; MONITOR_V = [MONITOR_V;V(2,:)];
   MONITOR_Z = [MONITOR_Z;V(3,:)];
   ITER_MAXZ = [ITER,max(abs(V(3,:)))]
   %pause(0.2)
end
switch Example
   case 1, save daten5b MONITOR_U MONITOR_V MONITOR_Z T
   case 2, save daten5c MONITOR_U MONITOR_V MONITOR_Z T
   case 3, save daten5d MONITOR_U MONITOR_V MONITOR_Z T
   case 4, save daten5e MONITOR_U MONITOR_V MONITOR_Z T
end
%disp(' bild05 Aufrufen! ')
bild05

