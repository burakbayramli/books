function demo2
% Channel problems with constant water depth
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
% NN      Number of periods, Condition NN*Period = 3600 
% ---------------------------------
clc, clear, format compact, format short
Example = 100; while ~ismember(Example,[1,2,3,4])
Example = input(' Which example? (1/2/3/4) '); end
%Example = 2;
Start = 100; while ~ismember(Start,[0,1])
Start = input(' Cold start/restart? (1/0) '); end
switch Example
case 1, disp(' Channel length 160 m, Period: 200 Sec. ')
   FF1 = 'bsp02'; FF2 = 'bsp02h';
   XSCALE  = 4; YSCALE  = 10; L = 160; H = 2;
   PERIOD  = 200; A = 0.1; DT = 10;
   HOURS = PERIOD/3600; NN = 1; 
case 2, disp(' Channel length 1000 m, diverse periods')
   FF1 = 'bsp02'; FF2 = 'bsp02h1';
   XSCALE  = 25; YSCALE  = 100; L = 1000; H = 20;
   PERIOD  = 360; % PERIOD =  180/360/540/720 % [sec] cycle for inlet
   A = 1; DT = 60;
   HOURS = 1; NN = HOURS*3600/PERIOD; 
case 3, disp(' Channel length 4000 m')
   disp(' see also demo8.m ') % cf. Ninomiya, Bsp. 9.4.1
   FF1 = 'bsp02'; FF2 = 'bsp02h1';
   XSCALE  = 100; YSCALE  = 100; L = 4000; H = 20;
   PERIOD  = 3600; A = 0.5; DT = 20; 
   HOURS = 1; NN = HOURS*3600/PERIOD; 
case 4, disp(' Channel length 1000 m')
   disp(' see also demo5.m ') % cf. Ninomiya, Bsp. 9.4.1
   FF1 = 'bsp02'; FF2 = 'bsp02h1';
   XSCALE  = 25; YSCALE  = 100; L = 1000; H = 20;
   PERIOD  = 3600; A = 1; DT = 60; 
   HOURS = 1; NN = HOURS*3600/PERIOD; 
end
NU_E = 2.3*1E-2; % Eddy viscosity/RHO 
g    = 10; % 9.81 grav. acceleration [m/sec^2]
TAU  = 0;  % TAU = 1/C^2, C Chezy coefficient (not used)
tspan     = [0,DT];     % Time interval
MAXITER   = HOURS*3600/DT

% -- Geometrical data of channel --------------
[p,e,t] = feval(FF1);  % first mesh
%[p,e,t] = mesh01_t([],p,e,t); % doubtful
p(1,:)  = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:);
N = size(p,2); p = [p;H*ones(1,N)]; 
%bild00(p,e,t), pause

MASSMATRIX = massfun(p,e,t);
MASSMATRIX1 = kron(speye(3),MASSMATRIX);

%options = odeset('Reltol',1E-5,'mass',MASSMATRIX1); %ODE23T.M
options = odeset('Reltol',1E-5);                    % ODE23.M 
%options = odeset('Reltol',1E-7,'mass',MASSMATRIX1); % ODE23TB.M
%options = odeset('Reltol',1E-5);                    % ODE34.M
switch Start
case 1
   X0 = zeros(3*N,1);  % Cold start 
   MONITOR_U = zeros(1,N); MONITOR_V = zeros(1,N); MONITOR_Z = zeros(1,N);
   Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN,NU_E];
   save daten2a p e t Parmeter FF2 Example HOURS
case 0
   load daten2a p e t Parmeter FF2 Example HOURS
   switch Example
      case 1, load daten2b MONITOR_U MONITOR_V MONITOR_Z
      case 2, load daten2c MONITOR_U MONITOR_V MONITOR_Z
      case 3, load daten2d MONITOR_U MONITOR_V MONITOR_Z
      case 4, load daten2e MONITOR_U MONITOR_V MONITOR_Z
   end
   AUXU = MONITOR_U(end,:); AUXV = MONITOR_V(end,:);
   AUXZ = MONITOR_Z(end,:);
   X0 = [AUXU,AUXV,AUXZ].';
   HOURS = HOURS + Parmeter(11);
   save daten2a p e t Parmeter FF2 Example HOURS
end
% --- Simulation ---------------
for ITER = 1:MAXITER
   % [T,Y] = ode23t(@rside10,tspan,X0,options,Parmeter,p,e,t,FF2);
   [T,Y] = ode23(@rside10,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
  % [T,Y] = ode23(@rside11,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);

   % [T,Y] = ode23tb(@rside10,tspan,X0,options,Parmeter,p,e,t,FF2);
   % [T,Y] = ode45(@rside10,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
   [LM,LN] = size(Y); TT = T(LM);
   U = Y(LM,1:N); V = Y(LM,N+1:2*N); Z = Y(LM,2*N+1:3*N);
   % -- Insert boundary conditions  ---------------
   [RDU,RDV,RDZ] = feval(FF2,e,TT,Parmeter);
   U(RDU(1,:)) = RDU(2,:); V(RDV(1,:)) = RDV(2,:);
   Z(RDZ(1,:)) = RDZ(2,:);
   tspan = tspan + DT;
   %%%% Correcture for channel (necessary) %%%%%%%%%%%%%%
   V = 0*V; 
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   X0 = [U,V,Z].';
   MONITOR_U = [MONITOR_U;U]; MONITOR_V = [MONITOR_V;V];
   MONITOR_Z = [MONITOR_Z;Z];
   MAXITER_ITER_MAXZ = [MAXITER,ITER,max(abs(Z))]
end
switch Example
   case 1, save daten2b MONITOR_U MONITOR_V MONITOR_Z 
   case 2, save daten2c MONITOR_U MONITOR_V MONITOR_Z 
   case 3, save daten2d MONITOR_U MONITOR_V MONITOR_Z 
   case 4, save daten2e MONITOR_U MONITOR_V MONITOR_Z
end
%disp(' bild02 Aufrufen! ')
bild02
