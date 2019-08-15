function demo8
% Masterfile for shallow water problem
% Channel problem, constant mean depth
% several different methods
% p = (p1;p2;): (X,Y)-Coordinates
% U/V/Z : Velocity in x/y-direction,Tidal elevation
% ---------------------------------
clc, clear, format short, format compact
disp(' Canal length 4000 m, Period = 3600 Sek.')
% vgl. Ninomiya, Bsp. 9.4.1
%%%%% Parameter %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FF1 = 'bsp02'; FF2 = 'bsp02h1'; XSCALE  = 100; YSCALE  = 100;

%% besser als BSP02H1.M %%%%%%%%%%%%%%%%%%%%%%%%%%%%
HOURS  =  8;  % total 8, simulation time total [hour]
HOURS = 2;
DT      = 60; % Time step [sec]

PERIOD  = 3600;  % Period at open left end [sec]
L       = 4000;  % Canal length [m]
H       = 20;    % Water level [m]
A       = 0.5;  % Amplitude [m]
NU_E    = 2.3*1E-2;   % Eddy viscosity, vgl. Nino.
g       = 10;   % 9.81 gravitational acceleration [m/sec^2]
TAU     = 0;    % TAU = 1/C^2, C Chezy coefficient
%%% End of parameter input %%%%%%%%%%%%%%%%%%%%%%5
% Iteration over NN Periods
NN      = HOURS*3600/PERIOD; 
MAXITER = NN*PERIOD/DT; % DT*MAXITER = NN Perioden
%%% mesh generation %%%%%%%%%%%%%%%%%%%%%%%%
[p,e,t] = feval(FF1);  % erstes Gitter
%[p,e,t] = mesh01_t([],p,e,t);

p(1,:)  = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:);
N       = size(p,2);  
MIDPOINTS = find(p(2,:) == 800);
%%% Computation of mass matrix %%%%%%%%%%%%%%
%[MM1,MM2,MASSMATRIX]  = massfun(p,e,t);
MASSMATRIX  = massfun(p,e,t);


MASSMATRIX1 = kron(speye(3),MASSMATRIX);
I = find(e(5,:) == 1); LI = length(I); % Boundary segment 1
JJ = [e(1,I(1:LI)), e(2,I(LI))];       % for Monitoring
%bild00(p,e,t)
%pause

Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN,NU_E];
save daten8a p e t Parmeter FF2 MAXITER
%%% Start values (Cold Start) %%%%%%%%%%%%%%%%
T  = 0; U0 = zeros(N,1); V0 = U0; Z0 = U0;
X0 = [U0;V0;Z0];
MONITOR_U = U0(JJ).'; MONITOR_Z = Z0(JJ).';
MONITOR_V = V0(MIDPOINTS).';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tspan = [0,DT];
disp(' Adapt ODESET ! ')
%options = odeset('Reltol',1E-5,'mass',MASSMATRIX1); %ODE23T.M/ODE23TB.M
options = odeset('Reltol',1E-5);
% -------------------------------------
tic
% Simulation start --------------------
for ITER = 1:MAXITER
   %   [T,Y] = ode23t(@rside,tspan,X0,options,Parmeter,p,e,t,FF2);
      [T,Y] = ode23(@rside9,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
   %   [T,Y] = ode23tb(@rside,tspan,X0,options,Parmeter,p,e,t,FF2);
   %   [T,Y] = ode45(@rside,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
   [LM,LN] = size(Y); TT = T(LM);
   U = Y(LM,1:N).'; V = Y(LM,N+1:2*N).'; Z = Y(LM,2*N+1:3*N).';
   tspan = tspan + DT;
   % -- Insert boundary conditions (undispensable) ----
   [RDU,RDV,RDZ] = feval(FF2,e,TT,Parmeter);
   U(RDU(1,:)) = RDU(2,:)'; V(RDV(1,:)) = RDV(2,:)';
   Z(RDZ(1,:)) = RDZ(2,:)';
   %%%% Correcture for channel (necessary) %%%%%%%%%%%%%%
   %V = 0*V; 
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   X0 = [U;V;Z];
   MONITOR_U = [MONITOR_U;U(JJ).']; MONITOR_Z = [MONITOR_Z;Z(JJ).'];
   MONITOR_V = [MONITOR_V;V(MIDPOINTS).'];

   MAX_ITER_MAXZ = [MAXITER,ITER,max(abs(Z(JJ)))]
end
ZEIT = toc
save daten8b MONITOR_U MONITOR_Z MONITOR_V
disp(' Call bild08 ! ')
