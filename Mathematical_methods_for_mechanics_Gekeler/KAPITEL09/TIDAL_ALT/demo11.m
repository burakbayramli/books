function demo11
% wie demo8.m aber integration ueber volle distanz
% Masterfile for shallow water problem
% Channel problem, constant mean depth
% several different methods
% p = (p1;p2;): (X,Y)-Coordinates
% U/V/Z : Velocity in x/y-direction,Tidal elevation
% ---------------------------------
clc, clear, format short, format compact
%disp(' Canal length 4000 m, Period = 3600 Sek.')
% vgl. Ninomiya, Bsp. 9.4.1
%%%%% Parameter %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FF1 = 'bsp02'; FF2 = 'bsp02h1'; XSCALE  = 100; YSCALE  = 100;

%% besser als BSP02H1.M %%%%%%%%%%%%%%%%%%%%%%%%%%%%
HOURS  =  8;  % total 8, simulation time total [hour]
HOURS = 3;
DT = 10;
PERIOD  = 3600;  % Period at open left end [sec]
L       = 4000;  % Canal length [m]
H       = 20;    % Water level [m]
A       = 0.5;  % Amplitude [m]
NU_E    = 2.3*1E-2;   % Eddy viscosity, vgl. Nino.
g       = 10;   % 9.81 gravitational acceleration [m/sec^2]
TAU     = 0;    % TAU = 1/C^2, C Chezy coefficient

%NU_E = 0;


XSCALE = 25;
YSCALE = 25;
L = 1000;
PERIOD = 3600;
A = 1;
HOURS = 1;
DT = 0.5;
%NU_E = 0;




%%% End of parameter input %%%%%%%%%%%%%%%%%%%%%%5
% Iteration over NN Periods
NN      = HOURS*3600/PERIOD; 
%%% mesh generation %%%%%%%%%%%%%%%%%%%%%%%%
[p,e,t] = feval(FF1);  % erstes Gitter
%[p,e,t] = mesh01_t([],p,e,t);

p(1,:)  = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:);
N       = size(p,2);  
MIDPOINTS = find(p(2,:) == 800);
%%% Computation of mass matrix %%%%%%%%%%%%%%
MASSMATRIX  = massfun(p,e,t);
MASSMATRIX1 = kron(speye(3),MASSMATRIX);
%bild00(p,e,t)
%pause

Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN,NU_E];
save daten11a p e t Parmeter FF2 
%%% Start values (Cold Start) %%%%%%%%%%%%%%%%
T  = 0; U0 = zeros(N,1); V0 = U0; Z0 = U0;
X0 = [U0;V0;Z0];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tspan = linspace(0,HOURS*3600,120);
disp(' Adapt ODESET ! ')
options = odeset('Reltol',1E-5,'mass',MASSMATRIX1); %ODE23T.M/ODE23TB.M
options = odeset('Reltol',1E-3,'Stats','on');
% -------------------------------------
tic
% Simulation start --------------------
   %   [T,Y] = ode23t(@rside,tspan,X0,options,Parmeter,p,e,t,FF2);
      [T,Y] = ode23(@rside11,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
   %   [T,Y] = ode23tb(@rside,tspan,X0,options,Parmeter,p,e,t,FF2);
   %   [T,Y] = ode45(@rside,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
   U = Y(:,1:N); V = Y(:,N+1:2*N); Z = Y(:,2*N+1:3*N);
   save daten11 p e t T U V Z
   bild11
