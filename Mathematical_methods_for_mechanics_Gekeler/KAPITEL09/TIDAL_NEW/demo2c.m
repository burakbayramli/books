function demo2c
% same as demo2b.m but with bsp03.m
% Channel problems with constant water depth
% channel closed at right
% p = (p1;p2;) (X,Y)-Coordinates
% U/V/Z   Velocity in x/y-direction, Water elevation
% XSCALE  scale factor in X-axis,
% YSCALE  scale factor in Y-axis
% L       Channel length [m]
% H       Water depth [m]
% PERIOD  Period at open (left) end [sec]
% HOURS   Simulation time total [hour]
% A       Amplitude [m]
% DT      Time step [sec]
% ---------------------------------
clc, clear, format compact, format short
%Example = 100; while ~ismember(Example,[1,2,3,4])
%Example = input(' Which example? (1/2/3/4) '); end

Example = 3;

Start = 100; while ~ismember(Start,[0,1])
Start = input(' Cold start/restart? (1/0) '); end
switch Example
case 1, disp(' Channel length 160 m, Period: 200 Sec. ')
   FF1 = 'bsp03'; FF2 = 'bsp02h'; L = 160; H = 2;
   XSCALE  = 1/18.75; YSCALE  = 160/200; ZSCALE = 1/5;
   PERIOD  = 200; A = 0.1; DT = 0.1;
   HOURS = PERIOD/3600; NN = HOURS*3600/PERIOD; % number of periods
case 2, disp(' Channel length 1000 m, diverse periods')
   FF1 = 'bsp03'; FF2 = 'bsp02h'; L = 1000; H = 20;
   XSCALE  = 1/3; YSCALE  = 1; ZSCALE = 2; 
   PERIOD  = 360; % PERIOD =  180/360/540/720 % [sec] cycle for inlet
   A = 1; DT = 1;
   HOURS = 1; NN = HOURS*3600/PERIOD; 
case 3, disp(' Channel length 1000 m') %same result as in demo2b!!
   FF1 = 'bsp03'; FF2 = 'bsp02h';
   XSCALE  = 1/3; YSCALE  = 1; ZSCALE = 2;
   PERIOD  = 3600; A = 0.5; L = 1000; H = 20;
   DT = 0.1; 
   HOURS = 0.5; NN = HOURS*3600/PERIOD; % number of periods
case 4, disp(' Channel length 4000 m')
   FF1 = 'bsp03'; FF2 = 'bsp02h'; 
   XSCALE  = 4/3; YSCALE  = 8; ZSCALE = 2; 
   PERIOD  = 3600; A = 0.5; L = 4000; H = 20;
   DT = 1;
   HOURS = 1; NN = HOURS*3600/PERIOD; 
end
MAXITER   = HOURS*3600/DT;

nu    = 23; % Eddy viscosity/RHO [m^2/s], Nino. p. 168
g     = 10; % 9.81 grav. acceleration [m/sec^2]
n     = 0.025; 
C     = H^(1/6)/n;     %Chezy coefficient
kappa = g/(H*C^2); %factor for friction after Chezy
nu = 0;
kappa = 0;
Parmeter = [A,PERIOD,g,nu,kappa];

% -- Geometrical data of channel --------------
[p,e,t] = feval(FF1);  % first mesh
%[p,e,t] = mesh01_t([],p,e,t); % doubtful
p(1,:) = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:); p(3,:) = ZSCALE*p(3,:);
N = size(p,2); p = [p(1:2,:);H*ones(1,N)]; 
[MM,MME,INVLUMP,LUMP] = massfun(p,e,t);
%bild00(p,e,t), pause

switch Start
case 1
   V = zeros(3,N);  % Cold start 
   T = 0;
   MONITOR_U = zeros(1,N); MONITOR_V = zeros(1,N); MONITOR_Z = zeros(1,N);
   save daten2a1 p e t FF2 Example HOURS Parmeter
case 0
   load daten2a p e t FF2 Example HOURS Parmeter 
   T = HOURS*3600;
   switch Example
      case 1, load daten2b1 MONITOR_U MONITOR_V MONITOR_Z DT
      case 2, load daten2c1 MONITOR_U MONITOR_V MONITOR_Z DT
      case 3, load daten2d1 MONITOR_U MONITOR_V MONITOR_Z DT
      case 4, load daten2e1 MONITOR_U MONITOR_V MONITOR_Z DT
   end
   AUXU = MONITOR_U(end,:); AUXV = MONITOR_V(end,:);
   AUXZ = MONITOR_Z(end,:);
   V = [AUXU;AUXV;AUXZ];
   HOURS = HOURS + 1;
   save daten2a1 p e t FF2 Example HOURS Parmeter
end
% --- Simulation ---------------
for ITER = 1:MAXITER
 % VN = V + (0.5*DT*rside_1(V,p,e,t,Parmeter))*INVLUMP; %good
   VN = V + (0.5*DT*rside_1(V,p,e,t,Parmeter))/MM;  % better  
 % VN = (V*MM + 0.5*DT*rside_1(V,p,e,t,Parmeter))*INVLUMP; %wrong
   VN = (V*MME + 0.5*DT*rside_1(V,p,e,t,Parmeter))*INVLUMP;  % good
   T   = T + DT/2;
   [RDU,RDV,RDZ] = feval(FF2,e,T,A,PERIOD);
   VN(1,RDU(1,:)) = RDU(2,:); VN(2,RDV(1,:)) = RDV(2,:);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   %%%% Correcture for straight channel (necessary) %%%%%%%%%%%%%%
   VN(2,:) = 0; 
   % ------------------------------
 % VN = V + (DT*rside_1(V,p,e,t,Parmeter))*INVLUMP; %good
   VN = V + (DT*rside_1(V,p,e,t,Parmeter))/MM;  % better  
 % VN = (V*MM + DT*rside_1(V,p,e,t,Parmeter))*INVLUMP; %wrong
   VN = (V*MME + DT*rside_1(V,p,e,t,Parmeter))*INVLUMP;  % good
   T   = T + DT/2;
   [RDU,RDV,RDZ] = feval(FF2,e,T,A,PERIOD);
   VN(1,RDU(1,:)) = RDU(2,:); VN(2,RDV(1,:)) = RDV(2,:);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   %%%% Correcture for channel (necessary) %%%%%%%%%%%%%%
   VN(2,:) = 0; 
   V = VN;
   MONITOR_U = [MONITOR_U;V(1,:)];
   MONITOR_V = [MONITOR_V;V(2,:)];
   MONITOR_Z = [MONITOR_Z;V(3,:)];
   MAXITER_ITER_MAXZ = [MAXITER,ITER,abs(V(3,92))]
end
switch Example
   case 1, save daten2b1 MONITOR_U MONITOR_V MONITOR_Z DT
   case 2, save daten2c1 MONITOR_U MONITOR_V MONITOR_Z DT
   case 3, save daten2d1 MONITOR_U MONITOR_V MONITOR_Z DT
   case 4, save daten2e1 MONITOR_U MONITOR_V MONITOR_Z DT
end
disp(' Call bild02c ! ')
