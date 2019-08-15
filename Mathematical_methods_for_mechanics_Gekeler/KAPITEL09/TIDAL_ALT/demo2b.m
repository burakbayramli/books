function demo2b
% Masterfile for shallow water problem
% Channel problem, constant mean depth
% p = (p1;p2;): (X,Y)-Coordinates
% U/V/Z : Velocity in x/y-direction,Tidal elevation
% Alle Verfahren bringen das gleiche Ergebnis
% am schnellsten ODE23.M
% ---------------------------------
clc, clear, format compact, format short
Beispiel = 100;
while ~ismember(Beispiel,[1,2,3,4])
   Beispiel = input(' Welches Beispiel? (1/2/3)');
end
switch Beispiel
case 1, disp(' Kanallaenge 160 m, Periode: 200 Sec. ')
   FF1 = 'bsp02'; FF2 = 'bsp02h';
   XSCALE  = 4;    % scale factor in X-axis,
   YSCALE  = 10;   % scale factor in Y-axis
   L       = 160;  % Kanallaenge [m]
   H       = 2;    % Wasserstand [m]
   PERIOD  = 200;  % Periode am offenen Ende [sec]
   A       = 0.1;  % Amplitude [m]
   DT      = 10; % Zeitschritt [sec]
   NN      = 1; % Anzahl Perioden
case 2, disp(' Kanallaenge 1000 m, versch. Perioden')
   FF1 = 'bsp02'; FF2 = 'bsp02h1';
   HOURS  =  1;  % simulation time total [hour]
   XSCALE  = 25;    % scale factor in X-axis,
   YSCALE  = 100;  % scale factor in Y-axis
   L       = 1000;  % Kanallaenge [m]
   H       = 20;    % Wasserstand [m]
   PERIOD  = 360;  % Periode am offenen linken Ende [sec]
   % PERIOD =  180/360/540/720 % [sec] cycle for inlet
   A       = 1;  % Amplitude [m]
   DT      = 60; % Zeitschritt [sec]
   % Iteration ueber NN Perioden
   NN = HOURS*3600/PERIOD; 
case 3, disp(' Kanallaenge 4000 m')
   disp(' see also demo8.m ') % vgl. Ninomiya, Bsp. 9.4.1
   FF1 = 'bsp02'; FF2 = 'bsp02h1';
   HOURS  =  1.5;  % simulation time total [hour]
   HOURS = 5;
   XSCALE  = 100;    % scale factor in X-axis,
   YSCALE  = 100;   % scale factor in Y-axis
   L       = 4000;  % Kanallaenge [m]
   H       = 20;    % Wasserstand [m]
   PERIOD  = 3600;  % Periode am offenen linken Ende [sec]
   A       = 0.5;  % Amplitude [m]
   DT      = 20; % Zeitschritt [sec]
   % Iteration ueber NN Perioden
   NN = HOURS*3600/PERIOD; 
end
NU_E = 2.3*1E-2; % Eddy viscosity/RHO 
g    = 10; % 9.81 Erdbeschleunigung [m/sec^2]
TAU  = 0;  % TAU = 1/C^2, C Chezy coefficient

[p,e,t] = feval(FF1);  % erstes Gitter
%[p,e,t] = mesh01_t([],p,e,t); % same result in Bsp. 2
p(1,:)  = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:);
N = size(p,2); p = [p;H*ones(1,N)]; 
%bild00(p,e,t)
%pause
MASSMATRIX = massfun(p,e,t);
MASSMATRIX1 = kron(speye(3),MASSMATRIX);

%options = odeset('Reltol',1E-5,'mass',MASSMATRIX1); %ODE23T.M
options = odeset('Reltol',1E-5);                    % ODE23.M 
%options = odeset('Reltol',1E-7,'mass',MASSMATRIX1); % ODE23TB.M
%options = odeset('Reltol',1E-5); 
                   % ODE34.M
X0 = zeros(3*N,1);  % Cold start 
MONITOR_Z = zeros(1,N); MONITOR_U = zeros(1,N);
tspan     = [0,DT];     % Time interval
MAXITER   = NN*PERIOD/DT; % DT*MAXITER = NN Perioden
Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN,NU_E];
save daten5a p e t Parmeter FF2 MAXITER Beispiel

for ITER = 1:MAXITER
   % [T,Y] = ode23t(@rside,tspan,X0,options,Parmeter,p,e,t,FF2);
   [T,Y] = ode23(@rside10,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
   % [T,Y] = ode23tb(@rside3,tspan,X0,options,Parmeter,p,e,t,FF2);
   % [T,Y] = ode45(@rside4,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
   [LM,LN] = size(Y); TT = T(LM);
   U = Y(LM,1:N); V = Y(LM,N+1:2*N); Z = Y(LM,2*N+1:3*N);
   % -- Randwerte Einfuegen ---------------
   [RDU,RDV,RDZ] = feval(FF2,e,TT,Parmeter);
   U(RDU(1,:)) = RDU(2,:); V(RDV(1,:)) = RDV(2,:);
   Z(RDZ(1,:)) = RDZ(2,:);
   tspan = tspan + DT;
   %%%% Correcture for channel (necessary) %%%%%%%%%%%%%%
   V = 0*V; 
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   X0 = [U,V,Z].';
   MONITOR_U = [MONITOR_U;U]; MONITOR_Z = [MONITOR_Z;Z];
   MAXITER_ITER_MAXZ = [MAXITER,ITER,max(abs(Z))]
end
switch Beispiel
   case 1, save daten5b MONITOR_U MONITOR_Z MAXITER
   case 2, save daten5c MONITOR_U MONITOR_Z MAXITER
   case 3, save daten5d MONITOR_U MONITOR_Z MAXITER
end
disp(' bild05 Aufrufen! ')
%bild05
